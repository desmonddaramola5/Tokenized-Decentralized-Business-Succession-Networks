;; Tax Optimization Contract
;; Minimizes succession-related tax obligations and manages tax planning

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-STRATEGY (err u401))
(define-constant ERR-INVALID-CALCULATION (err u402))
(define-constant ERR-STRATEGY-EXISTS (err u403))
(define-constant ERR-INSUFFICIENT-DATA (err u404))

;; Data Variables
(define-data-var next-strategy-id uint u1)
(define-data-var next-calculation-id uint u1)

;; Tax rates (simplified - in basis points, 10000 = 100%)
(define-data-var estate-tax-rate uint u4000) ;; 40%
(define-data-var gift-tax-rate uint u4000) ;; 40%
(define-data-var capital-gains-rate uint u2000) ;; 20%

;; Data Maps
(define-map tax-strategies
  { strategy-id: uint }
  {
    business-id: uint,
    strategy-type: (string-ascii 50),
    created-by: principal,
    estimated-savings: uint,
    implementation-cost: uint,
    timeline: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map tax-calculations
  { calculation-id: uint }
  {
    business-id: uint,
    calculation-type: (string-ascii 50),
    business-value: uint,
    tax-liability: uint,
    potential-savings: uint,
    calculated-by: principal,
    calculated-at: uint,
    valid-until: uint
  }
)

(define-map optimization-methods
  { method-id: (string-ascii 50) }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    applicable-scenarios: (list 10 (string-ascii 50)),
    average-savings: uint,
    complexity-score: uint
  }
)

(define-map business-tax-profile
  { business-id: uint }
  {
    current-valuation: uint,
    annual-revenue: uint,
    profit-margin: uint,
    jurisdiction: (string-ascii 50),
    entity-type: (string-ascii 50),
    last-updated: uint
  }
)

(define-map certified-tax-advisors
  { advisor: principal }
  {
    certified: bool,
    specialties: (list 5 (string-ascii 50)),
    jurisdictions: (list 10 (string-ascii 50)),
    success-rate: uint,
    total-strategies: uint
  }
)

;; Public Functions

;; Create tax optimization strategy
(define-public (create-tax-strategy
  (business-id uint)
  (strategy-type (string-ascii 50))
  (estimated-savings uint)
  (implementation-cost uint)
  (timeline uint))
  (let
    (
      (strategy-id (var-get next-strategy-id))
      (advisor-info (unwrap! (map-get? certified-tax-advisors { advisor: tx-sender }) ERR-NOT-AUTHORIZED))
    )
    (asserts! (get certified advisor-info) ERR-NOT-AUTHORIZED)
    (asserts! (> estimated-savings u0) ERR-INVALID-STRATEGY)
    (asserts! (> timeline u0) ERR-INVALID-STRATEGY)

    (map-set tax-strategies
      { strategy-id: strategy-id }
      {
        business-id: business-id,
        strategy-type: strategy-type,
        created-by: tx-sender,
        estimated-savings: estimated-savings,
        implementation-cost: implementation-cost,
        timeline: timeline,
        status: "proposed",
        created-at: block-height
      }
    )

    (var-set next-strategy-id (+ strategy-id u1))
    (ok strategy-id)
  )
)

;; Calculate tax liability
(define-public (calculate-tax-liability
  (business-id uint)
  (calculation-type (string-ascii 50))
  (business-value uint))
  (let
    (
      (calculation-id (var-get next-calculation-id))
      (tax-liability (calculate-liability calculation-type business-value))
      (potential-savings (calculate-potential-savings calculation-type business-value))
    )
    (asserts! (> business-value u0) ERR-INVALID-CALCULATION)

    (map-set tax-calculations
      { calculation-id: calculation-id }
      {
        business-id: business-id,
        calculation-type: calculation-type,
        business-value: business-value,
        tax-liability: tax-liability,
        potential-savings: potential-savings,
        calculated-by: tx-sender,
        calculated-at: block-height,
        valid-until: (+ block-height u4380) ;; ~30 days
      }
    )

    (var-set next-calculation-id (+ calculation-id u1))
    (ok calculation-id)
  )
)

;; Update business tax profile
(define-public (update-tax-profile
  (business-id uint)
  (current-valuation uint)
  (annual-revenue uint)
  (profit-margin uint)
  (jurisdiction (string-ascii 50))
  (entity-type (string-ascii 50)))
  (begin
    (asserts! (> current-valuation u0) ERR-INVALID-CALCULATION)
    (asserts! (<= profit-margin u10000) ERR-INVALID-CALCULATION) ;; Max 100%

    (map-set business-tax-profile
      { business-id: business-id }
      {
        current-valuation: current-valuation,
        annual-revenue: annual-revenue,
        profit-margin: profit-margin,
        jurisdiction: jurisdiction,
        entity-type: entity-type,
        last-updated: block-height
      }
    )
    (ok true)
  )
)

;; Approve tax strategy
(define-public (approve-strategy (strategy-id uint))
  (let
    (
      (strategy (unwrap! (map-get? tax-strategies { strategy-id: strategy-id }) ERR-INVALID-STRATEGY))
    )
    ;; In real implementation, would check business ownership
    (map-set tax-strategies
      { strategy-id: strategy-id }
      (merge strategy { status: "approved" })
    )
    (ok true)
  )
)

;; Register optimization method
(define-public (register-optimization-method
  (method-id (string-ascii 50))
  (name (string-ascii 100))
  (description (string-ascii 500))
  (applicable-scenarios (list 10 (string-ascii 50)))
  (average-savings uint)
  (complexity-score uint))
  (let
    (
      (advisor-info (unwrap! (map-get? certified-tax-advisors { advisor: tx-sender }) ERR-NOT-AUTHORIZED))
    )
    (asserts! (get certified advisor-info) ERR-NOT-AUTHORIZED)
    (asserts! (<= complexity-score u100) ERR-INVALID-STRATEGY)

    (map-set optimization-methods
      { method-id: method-id }
      {
        name: name,
        description: description,
        applicable-scenarios: applicable-scenarios,
        average-savings: average-savings,
        complexity-score: complexity-score
      }
    )
    (ok true)
  )
)

;; Certify tax advisor
(define-public (certify-tax-advisor
  (advisor principal)
  (specialties (list 5 (string-ascii 50)))
  (jurisdictions (list 10 (string-ascii 50))))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)

    (map-set certified-tax-advisors
      { advisor: advisor }
      {
        certified: true,
        specialties: specialties,
        jurisdictions: jurisdictions,
        success-rate: u50,
        total-strategies: u0
      }
    )
    (ok true)
  )
)

;; Private Functions

(define-private (calculate-liability (calculation-type (string-ascii 50)) (business-value uint))
  (if (is-eq calculation-type "estate")
    (/ (* business-value (var-get estate-tax-rate)) u10000)
    (if (is-eq calculation-type "gift")
      (/ (* business-value (var-get gift-tax-rate)) u10000)
      (/ (* business-value (var-get capital-gains-rate)) u10000)
    )
  )
)

(define-private (calculate-potential-savings (calculation-type (string-ascii 50)) (business-value uint))
  ;; Simplified calculation - 20% potential savings through optimization
  (/ (* business-value u2000) u10000)
)

;; Read-only Functions

(define-read-only (get-tax-strategy (strategy-id uint))
  (map-get? tax-strategies { strategy-id: strategy-id })
)

(define-read-only (get-tax-calculation (calculation-id uint))
  (map-get? tax-calculations { calculation-id: calculation-id })
)

(define-read-only (get-optimization-method (method-id (string-ascii 50)))
  (map-get? optimization-methods { method-id: method-id })
)

(define-read-only (get-business-tax-profile (business-id uint))
  (map-get? business-tax-profile { business-id: business-id })
)

(define-read-only (get-tax-advisor-info (advisor principal))
  (map-get? certified-tax-advisors { advisor: advisor })
)

(define-read-only (estimate-tax-savings (business-value uint) (strategy-type (string-ascii 50)))
  (let
    (
      (base-liability (calculate-liability "estate" business-value))
      (potential-savings (calculate-potential-savings "estate" business-value))
    )
    {
      base-liability: base-liability,
      potential-savings: potential-savings,
      net-benefit: (- potential-savings u50000) ;; Subtract estimated implementation cost
    }
  )
)
