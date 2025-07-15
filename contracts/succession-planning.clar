;; Succession Planning Contract
;; Develops ownership transition strategies and manages succession processes

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-PLAN (err u201))
(define-constant ERR-PLAN-EXISTS (err u202))
(define-constant ERR-INVALID-SUCCESSOR (err u203))
(define-constant ERR-PLAN-NOT-ACTIVE (err u204))

;; Data Variables
(define-data-var next-plan-id uint u1)
(define-data-var next-milestone-id uint u1)

;; Data Maps
(define-map succession-plans
  { plan-id: uint }
  {
    business-id: uint,
    owner: principal,
    successor: principal,
    plan-type: (string-ascii 50),
    transition-timeline: uint,
    ownership-percentage: uint,
    status: (string-ascii 20),
    created-at: uint,
    target-completion: uint
  }
)

(define-map plan-milestones
  { milestone-id: uint }
  {
    plan-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-date: uint,
    completed: bool,
    completed-at: (optional uint),
    required-for-transition: bool
  }
)

(define-map successor-qualifications
  { successor: principal, business-id: uint }
  {
    experience-years: uint,
    industry-knowledge: uint,
    leadership-score: uint,
    financial-literacy: uint,
    approved: bool,
    approved-by: (optional principal)
  }
)

(define-map transition-votes
  { plan-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

;; Public Functions

;; Create a succession plan
(define-public (create-succession-plan
  (business-id uint)
  (successor principal)
  (plan-type (string-ascii 50))
  (transition-timeline uint)
  (ownership-percentage uint))
  (let
    (
      (plan-id (var-get next-plan-id))
    )
    (asserts! (and (> ownership-percentage u0) (<= ownership-percentage u100)) ERR-INVALID-PLAN)
    (asserts! (> transition-timeline u0) ERR-INVALID-PLAN)
    (asserts! (not (is-eq tx-sender successor)) ERR-INVALID-SUCCESSOR)

    (map-set succession-plans
      { plan-id: plan-id }
      {
        business-id: business-id,
        owner: tx-sender,
        successor: successor,
        plan-type: plan-type,
        transition-timeline: transition-timeline,
        ownership-percentage: ownership-percentage,
        status: "draft",
        created-at: block-height,
        target-completion: (+ block-height transition-timeline)
      }
    )

    (var-set next-plan-id (+ plan-id u1))
    (ok plan-id)
  )
)

;; Add milestone to succession plan
(define-public (add-milestone
  (plan-id uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (target-date uint)
  (required-for-transition bool))
  (let
    (
      (plan (unwrap! (map-get? succession-plans { plan-id: plan-id }) ERR-INVALID-PLAN))
      (milestone-id (var-get next-milestone-id))
    )
    (asserts! (is-eq (get owner plan) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> target-date block-height) ERR-INVALID-PLAN)

    (map-set plan-milestones
      { milestone-id: milestone-id }
      {
        plan-id: plan-id,
        title: title,
        description: description,
        target-date: target-date,
        completed: false,
        completed-at: none,
        required-for-transition: required-for-transition
      }
    )

    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

;; Complete a milestone
(define-public (complete-milestone (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? plan-milestones { milestone-id: milestone-id }) ERR-INVALID-PLAN))
      (plan (unwrap! (map-get? succession-plans { plan-id: (get plan-id milestone) }) ERR-INVALID-PLAN))
    )
    (asserts! (or (is-eq (get owner plan) tx-sender) (is-eq (get successor plan) tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get completed milestone)) ERR-INVALID-PLAN)

    (map-set plan-milestones
      { milestone-id: milestone-id }
      (merge milestone {
        completed: true,
        completed-at: (some block-height)
      })
    )
    (ok true)
  )
)

;; Approve successor qualifications
(define-public (approve-successor
  (successor principal)
  (business-id uint)
  (experience-years uint)
  (industry-knowledge uint)
  (leadership-score uint)
  (financial-literacy uint))
  (begin
    (asserts! (and (<= industry-knowledge u100) (<= leadership-score u100) (<= financial-literacy u100)) ERR-INVALID-SUCCESSOR)

    (map-set successor-qualifications
      { successor: successor, business-id: business-id }
      {
        experience-years: experience-years,
        industry-knowledge: industry-knowledge,
        leadership-score: leadership-score,
        financial-literacy: financial-literacy,
        approved: true,
        approved-by: (some tx-sender)
      }
    )
    (ok true)
  )
)

;; Activate succession plan
(define-public (activate-plan (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? succession-plans { plan-id: plan-id }) ERR-INVALID-PLAN))
    )
    (asserts! (is-eq (get owner plan) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status plan) "draft") ERR-PLAN-NOT-ACTIVE)

    (map-set succession-plans
      { plan-id: plan-id }
      (merge plan { status: "active" })
    )
    (ok true)
  )
)

;; Vote on succession plan execution
(define-public (vote-on-succession (plan-id uint) (approve bool))
  (let
    (
      (plan (unwrap! (map-get? succession-plans { plan-id: plan-id }) ERR-INVALID-PLAN))
    )
    (asserts! (is-eq (get status plan) "active") ERR-PLAN-NOT-ACTIVE)

    (map-set transition-votes
      { plan-id: plan-id, voter: tx-sender }
      { vote: approve, voted-at: block-height }
    )
    (ok true)
  )
)

;; Read-only Functions

(define-read-only (get-succession-plan (plan-id uint))
  (map-get? succession-plans { plan-id: plan-id })
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? plan-milestones { milestone-id: milestone-id })
)

(define-read-only (get-successor-qualifications (successor principal) (business-id uint))
  (map-get? successor-qualifications { successor: successor, business-id: business-id })
)

(define-read-only (get-vote (plan-id uint) (voter principal))
  (map-get? transition-votes { plan-id: plan-id, voter: voter })
)

(define-read-only (is-plan-owner (plan-id uint) (user principal))
  (match (map-get? succession-plans { plan-id: plan-id })
    plan (is-eq (get owner plan) user)
    false
  )
)
