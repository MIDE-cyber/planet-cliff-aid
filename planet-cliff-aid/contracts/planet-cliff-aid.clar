;; PlanetCliffAid Impact Proof Token Platform

;; This contract implements:
;;   - Impact Proof Tokens (IPTs) as dynamic NFTs
;;   - Catalyst Contracts with measurable goals
;;   - Community Impact DAO voting
;;   - Milestone-based funding distribution
;;   - Outcome Oracle score submission
;;   - Retroactive funding after evaluation periods

;; ===== CONSTANTS =====

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-OWNER (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-UNAUTHORIZED (err u103))
(define-constant ERR-INVALID-PARAM (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-MILESTONE-CLOSED (err u106))
(define-constant ERR-EVALUATION-PENDING (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))

;; Evaluation period in blocks (~12 months at ~144 blocks/day)
(define-constant EVALUATION-PERIOD-BLOCKS u52560)

;; Platform fee in basis points (250 = 2.5%)
(define-constant PLATFORM-FEE-BPS u250)

;; Maximum impact score (0-1000)
(define-constant MAX-IMPACT-SCORE u1000)

;; ===== NFT DEFINITION =====

;; Impact Proof Token NFT
(define-non-fungible-token impact-proof-token uint)

;; ===== DATA MAPS AND VARS =====

;; Auto-incrementing counters
(define-data-var next-ipt-id uint u1)
(define-data-var next-initiative-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var platform-treasury uint u0)

;; IPT metadata and state
(define-map ipt-data
  { token-id: uint }
  {
    owner: principal,
    initiative-id: uint,
    impact-score: uint,       ;; 0-1000, updated by oracle
    minted-at: uint,          ;; block height
    last-updated: uint,       ;; block height of last oracle update
    total-rewards-claimed: uint
  }
)

;; Catalyst Contracts (community initiatives)
(define-map initiatives
  { initiative-id: uint }
  {
    creator: principal,
    title: (string-ascii 128),
    description: (string-ascii 512),
    target-amount: uint,         ;; in microSTX
    funded-amount: uint,
    milestone-count: uint,
    completed-milestones: uint,
    impact-score: uint,          ;; aggregated oracle score 0-1000
    created-at: uint,            ;; block height
    evaluation-start: uint,      ;; block height when evaluation began
    is-active: bool,
    is-evaluated: bool,
    retroactive-bonus-paid: bool
  }
)

;; Milestones within an initiative
(define-map milestones
  { initiative-id: uint, milestone-index: uint }
  {
    description: (string-ascii 256),
    required-score: uint,    ;; minimum oracle score to unlock
    payout-amount: uint,     ;; microSTX released on completion
    is-completed: bool,
    completed-at: uint
  }
)

;; Oracle score submissions (multi-source aggregation)
(define-map oracle-submissions
  { initiative-id: uint, oracle: principal }
  {
    score: uint,
    submitted-at: uint,
    source-type: (string-ascii 32)  ;; "sensor", "satellite", "community", "ml"
  }
)

;; Approved oracle addresses
(define-map approved-oracles
  { oracle: principal }
  { approved: bool, reputation: uint }
)

;; DAO proposals for initiative voting
(define-map dao-proposals
  { proposal-id: uint }
  {
    initiative-id: uint,
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    voting-ends-at: uint,
    executed: bool
  }
)

;; Track votes per voter per proposal
(define-map votes-cast
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

;; Voter reputation (used for reputation-weighted consensus)
(define-map voter-reputation
  { voter: principal }
  { reputation: uint }
)

;; Contributions to initiatives (for reward distribution)
(define-map contributions
  { initiative-id: uint, contributor: principal }
  { amount: uint, ipt-id: uint }
)

;; ===== PRIVATE FUNCTIONS =====

(define-private (calculate-fee (amount uint))
  (/ (* amount PLATFORM-FEE-BPS) u10000)
)

(define-private (get-reputation (voter principal))
  (default-to u1
    (get reputation (map-get? voter-reputation { voter: voter }))
  )
)

(define-private (is-approved-oracle (oracle principal))
  (default-to false
    (get approved (map-get? approved-oracles { oracle: oracle }))
  )
)

(define-private (mint-ipt (recipient principal) (initiative-id uint))
  (let (
    (token-id (var-get next-ipt-id))
  )
    (try! (nft-mint? impact-proof-token token-id recipient))
    (map-set ipt-data
      { token-id: token-id }
      {
        owner: recipient,
        initiative-id: initiative-id,
        impact-score: u0,
        minted-at: block-height,
        last-updated: block-height,
        total-rewards-claimed: u0
      }
    )
    (var-set next-ipt-id (+ token-id u1))
    (ok token-id)
  )
)

;; ===== READ-ONLY FUNCTIONS =====

(define-read-only (get-initiative (initiative-id uint))
  (map-get? initiatives { initiative-id: initiative-id })
)

(define-read-only (get-milestone (initiative-id uint) (milestone-index uint))
  (map-get? milestones { initiative-id: initiative-id, milestone-index: milestone-index })
)

(define-read-only (get-ipt-data (token-id uint))
  (map-get? ipt-data { token-id: token-id })
)

(define-read-only (get-contribution (initiative-id uint) (contributor principal))
  (map-get? contributions { initiative-id: initiative-id, contributor: contributor })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? dao-proposals { proposal-id: proposal-id })
)

(define-read-only (get-platform-treasury)
  (var-get platform-treasury)
)

(define-read-only (get-oracle-submission (initiative-id uint) (oracle principal))
  (map-get? oracle-submissions { initiative-id: initiative-id, oracle: oracle })
)

;; Check if a milestone is unlockable based on current initiative impact score
(define-read-only (is-milestone-unlockable (initiative-id uint) (milestone-index uint))
  (match (map-get? initiatives { initiative-id: initiative-id })
    initiative
      (match (map-get? milestones { initiative-id: initiative-id, milestone-index: milestone-index })
        milestone
          (and
            (not (get is-completed milestone))
            (>= (get impact-score initiative) (get required-score milestone))
          )
        false
      )
    false
  )
)

;; ===== PUBLIC FUNCTIONS =====

;; --- Oracle Management ---

(define-public (approve-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (map-set approved-oracles
      { oracle: oracle }
      { approved: true, reputation: u100 }
    )
    (ok true)
  )
)

(define-public (revoke-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (map-set approved-oracles
      { oracle: oracle }
      { approved: false, reputation: u0 }
    )
    (ok true)
  )
)

;; --- Initiative (Catalyst Contract) Creation ---

(define-public (create-initiative
  (title (string-ascii 128))
  (description (string-ascii 512))
  (target-amount uint)
)
  (let (
    (initiative-id (var-get next-initiative-id))
  )
    (asserts! (> target-amount u0) ERR-INVALID-PARAM)
    (asserts! (> (len title) u0) ERR-INVALID-PARAM)
    (map-set initiatives
      { initiative-id: initiative-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        target-amount: target-amount,
        funded-amount: u0,
        milestone-count: u0,
        completed-milestones: u0,
        impact-score: u0,
        created-at: block-height,
        evaluation-start: u0,
        is-active: true,
        is-evaluated: false,
        retroactive-bonus-paid: false
      }
    )
    (var-set next-initiative-id (+ initiative-id u1))
    (ok initiative-id)
  )
)

;; Add a milestone to an initiative
(define-public (add-milestone
  (initiative-id uint)
  (description (string-ascii 256))
  (required-score uint)
  (payout-amount uint)
)
  (match (map-get? initiatives { initiative-id: initiative-id })
    initiative
      (begin
        (asserts! (is-eq tx-sender (get creator initiative)) ERR-UNAUTHORIZED)
        (asserts! (get is-active initiative) ERR-MILESTONE-CLOSED)
        (asserts! (<= required-score MAX-IMPACT-SCORE) ERR-INVALID-PARAM)
        (asserts! (> payout-amount u0) ERR-INVALID-PARAM)
        (let (
          (milestone-index (get milestone-count initiative))
        )
          (map-set milestones
            { initiative-id: initiative-id, milestone-index: milestone-index }
            {
              description: description,
              required-score: required-score,
              payout-amount: payout-amount,
              is-completed: false,
              completed-at: u0
            }
          )
          (map-set initiatives
            { initiative-id: initiative-id }
            (merge initiative { milestone-count: (+ milestone-index u1) })
          )
          (ok milestone-index)
        )
      )
    ERR-NOT-FOUND
  )
)

;; --- Funding ---

;; Fund an initiative and receive an IPT
(define-public (fund-initiative (initiative-id uint) (amount uint))
  (match (map-get? initiatives { initiative-id: initiative-id })
    initiative
      (begin
        (asserts! (get is-active initiative) ERR-MILESTONE-CLOSED)
        (asserts! (> amount u0) ERR-INVALID-PARAM)
        (let (
          (fee (calculate-fee amount))
          (net-amount (- amount fee))
          (existing-contribution
            (map-get? contributions { initiative-id: initiative-id, contributor: tx-sender })
          )
        )
          ;; Transfer STX to this contract
          (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
          ;; Credit platform fee to treasury
          (var-set platform-treasury (+ (var-get platform-treasury) fee))
          ;; Update funded amount
          (map-set initiatives
            { initiative-id: initiative-id }
            (merge initiative { funded-amount: (+ (get funded-amount initiative) net-amount) })
          )
          ;; Mint IPT if first contribution, otherwise just log
          (if (is-none existing-contribution)
            (let (
              (token-id (try! (mint-ipt tx-sender initiative-id)))
            )
              (map-set contributions
                { initiative-id: initiative-id, contributor: tx-sender }
                { amount: net-amount, ipt-id: token-id }
              )
              (ok token-id)
            )
            (begin
              (map-set contributions
                { initiative-id: initiative-id, contributor: tx-sender }
                (merge (unwrap-panic existing-contribution)
                  { amount: (+ (get amount (unwrap-panic existing-contribution)) net-amount) }
                )
              )
              (ok (get ipt-id (unwrap-panic existing-contribution)))
            )
          )
        )
      )
    ERR-NOT-FOUND
  )
)

;; --- Outcome Oracle Network ---

;; Submit an impact score for an initiative
(define-public (submit-oracle-score
  (initiative-id uint)
  (score uint)
  (source-type (string-ascii 32))
)
  (begin
    (asserts! (is-approved-oracle tx-sender) ERR-UNAUTHORIZED)
    (asserts! (<= score MAX-IMPACT-SCORE) ERR-INVALID-PARAM)
    (asserts! (is-some (map-get? initiatives { initiative-id: initiative-id })) ERR-NOT-FOUND)
    (map-set oracle-submissions
      { initiative-id: initiative-id, oracle: tx-sender }
      {
        score: score,
        submitted-at: block-height,
        source-type: source-type
      }
    )
    (ok true)
  )
)

;; Aggregate oracle scores and update initiative impact score
;; This simplified aggregation uses the last submitted score from
;; the calling oracle and averages it with the existing score.
;; A production system would aggregate across all oracle submissions off-chain
;; and submit the final aggregated score here.
(define-public (update-initiative-score
  (initiative-id uint)
  (aggregated-score uint)
)
  (match (map-get? initiatives { initiative-id: initiative-id })
    initiative
      (begin
        (asserts! (is-approved-oracle tx-sender) ERR-UNAUTHORIZED)
        (asserts! (<= aggregated-score MAX-IMPACT-SCORE) ERR-INVALID-PARAM)
        (map-set initiatives
          { initiative-id: initiative-id }
          (merge initiative { impact-score: aggregated-score })
        )
        (ok aggregated-score)
      )
    ERR-NOT-FOUND
  )
)

;; --- Milestone Completion and Funding Distribution ---

;; Trigger payout for a completed milestone if oracle score threshold is met
(define-public (complete-milestone
  (initiative-id uint)
  (milestone-index uint)
)
  (match (map-get? initiatives { initiative-id: initiative-id })
    initiative
      (match (map-get? milestones { initiative-id: initiative-id, milestone-index: milestone-index })
        milestone
          (begin
            (asserts! (get is-active initiative) ERR-MILESTONE-CLOSED)
            (asserts! (not (get is-completed milestone)) ERR-MILESTONE-CLOSED)
            (asserts!
              (>= (get impact-score initiative) (get required-score milestone))
              ERR-EVALUATION-PENDING
            )
            (asserts!
              (>= (get funded-amount initiative) (get payout-amount milestone))
              ERR-INSUFFICIENT-FUNDS
            )
            ;; Mark milestone complete
            (map-set milestones
              { initiative-id: initiative-id, milestone-index: milestone-index }
              (merge milestone {
                is-completed: true,
                completed-at: block-height
              })
            )
            ;; Update initiative: deduct payout, increment completed milestones
            (map-set initiatives
              { initiative-id: initiative-id }
              (merge initiative {
                funded-amount: (- (get funded-amount initiative) (get payout-amount milestone)),
                completed-milestones: (+ (get completed-milestones initiative) u1)
              })
            )
            ;; Transfer payout to initiative creator
            (try!
              (as-contract
                (stx-transfer?
                  (get payout-amount milestone)
                  tx-sender
                  (get creator initiative)
                )
              )
            )
            (ok (get payout-amount milestone))
          )
        ERR-NOT-FOUND
      )
    ERR-NOT-FOUND
  )
)

;; --- Retroactive Funding ---

;; Begin 12-month evaluation period for an initiative
(define-public (start-evaluation-period (initiative-id uint))
  (match (map-get? initiatives { initiative-id: initiative-id })
    initiative
      (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
        (asserts! (get is-active initiative) ERR-MILESTONE-CLOSED)
        (asserts! (is-eq (get evaluation-start initiative) u0) ERR-ALREADY-EXISTS)
        (map-set initiatives
          { initiative-id: initiative-id }
          (merge initiative { evaluation-start: block-height })
        )
        (ok block-height)
      )
    ERR-NOT-FOUND
  )
)

;; Pay retroactive bonus after evaluation period if impact score is high
;; Bonus amount = 10% of funded amount for score >= 800
(define-public (pay-retroactive-bonus
  (initiative-id uint)
  (bonus-amount uint)
)
  (match (map-get? initiatives { initiative-id: initiative-id })
    initiative
      (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
        (asserts! (not (get retroactive-bonus-paid initiative)) ERR-ALREADY-EXISTS)
        (asserts! (> (get evaluation-start initiative) u0) ERR-EVALUATION-PENDING)
        (asserts!
          (>= block-height
            (+ (get evaluation-start initiative) EVALUATION-PERIOD-BLOCKS))
          ERR-EVALUATION-PENDING
        )
        ;; Require high impact score for bonus
        (asserts! (>= (get impact-score initiative) u800) ERR-INVALID-PARAM)
        (asserts! (>= (var-get platform-treasury) bonus-amount) ERR-INSUFFICIENT-FUNDS)
        ;; Deduct from treasury and pay creator
        (var-set platform-treasury (- (var-get platform-treasury) bonus-amount))
        (map-set initiatives
          { initiative-id: initiative-id }
          (merge initiative {
            is-evaluated: true,
            retroactive-bonus-paid: true
          })
        )
        (try!
          (as-contract
            (stx-transfer?
              bonus-amount
              tx-sender
              (get creator initiative)
            )
          )
        )
        (ok bonus-amount)
      )
    ERR-NOT-FOUND
  )
)

;; --- Community Impact DAO ---

;; Create a governance proposal to approve an initiative
(define-public (create-proposal
  (initiative-id uint)
  (voting-duration uint)
)
  (begin
    (asserts! (is-some (map-get? initiatives { initiative-id: initiative-id })) ERR-NOT-FOUND)
    (asserts! (> voting-duration u0) ERR-INVALID-PARAM)
    (let (
      (proposal-id (var-get next-proposal-id))
    )
      (map-set dao-proposals
        { proposal-id: proposal-id }
        {
          initiative-id: initiative-id,
          proposer: tx-sender,
          votes-for: u0,
          votes-against: u0,
          created-at: block-height,
          voting-ends-at: (+ block-height voting-duration),
          executed: false
        }
      )
      (var-set next-proposal-id (+ proposal-id u1))
      (ok proposal-id)
    )
  )
)

;; Vote on a DAO proposal with reputation-weighted voting
(define-public (cast-vote (proposal-id uint) (vote-for bool))
  (match (map-get? dao-proposals { proposal-id: proposal-id })
    proposal
      (begin
        (asserts! (<= block-height (get voting-ends-at proposal)) ERR-MILESTONE-CLOSED)
        (asserts! (is-none (map-get? votes-cast { proposal-id: proposal-id, voter: tx-sender }))
          ERR-ALREADY-VOTED
        )
        (let (
          (weight (get-reputation tx-sender))
        )
          (map-set votes-cast
            { proposal-id: proposal-id, voter: tx-sender }
            { vote: vote-for, weight: weight }
          )
          (if vote-for
            (map-set dao-proposals
              { proposal-id: proposal-id }
              (merge proposal { votes-for: (+ (get votes-for proposal) weight) })
            )
            (map-set dao-proposals
              { proposal-id: proposal-id }
              (merge proposal { votes-against: (+ (get votes-against proposal) weight) })
            )
          )
          (ok weight)
        )
      )
    ERR-NOT-FOUND
  )
)

;; Execute a passed proposal (activates/verifies the initiative)
(define-public (execute-proposal (proposal-id uint))
  (match (map-get? dao-proposals { proposal-id: proposal-id })
    proposal
      (begin
        (asserts! (> block-height (get voting-ends-at proposal)) ERR-EVALUATION-PENDING)
        (asserts! (not (get executed proposal)) ERR-ALREADY-EXISTS)
        (asserts!
          (> (get votes-for proposal) (get votes-against proposal))
          ERR-UNAUTHORIZED
        )
        (map-set dao-proposals
          { proposal-id: proposal-id }
          (merge proposal { executed: true })
        )
        (ok true)
      )
    ERR-NOT-FOUND
  )
)

;; --- Reputation Management ---

;; Update a voter's reputation (owner only, or could be automated via oracle outcomes)
(define-public (set-voter-reputation (voter principal) (reputation uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (map-set voter-reputation
      { voter: voter }
      { reputation: reputation }
    )
    (ok reputation)
  )
)

;; --- IPT NFT Transfers ---

(define-public (transfer-ipt (token-id uint) (recipient principal))
  (match (map-get? ipt-data { token-id: token-id })
    token
      (begin
        (asserts! (is-eq tx-sender (get owner token)) ERR-UNAUTHORIZED)
        (try! (nft-transfer? impact-proof-token token-id tx-sender recipient))
        (map-set ipt-data
          { token-id: token-id }
          (merge token { owner: recipient })
        )
        (ok true)
      )
    ERR-NOT-FOUND
  )
)

;; --- Platform Treasury Withdrawal (owner only) ---

(define-public (withdraw-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
    (asserts! (<= amount (var-get platform-treasury)) ERR-INSUFFICIENT-FUNDS)
    (var-set platform-treasury (- (var-get platform-treasury) amount))
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    (ok amount)
  )
)
