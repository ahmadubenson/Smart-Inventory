;; Smart Disposal and Waste Reduction Optimizer
;; Intelligent waste reduction system for supply chain optimization

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u500))
(define-constant err-item-not-found (err u501))
(define-constant err-invalid-parameters (err u502))
(define-constant err-recommendation-not-found (err u503))
(define-constant err-action-already-taken (err u504))

;; Action type constants
(define-constant action-dispose u1)
(define-constant action-markdown u2)
(define-constant action-donate u3)
(define-constant action-repurpose u4)
(define-constant action-return-supplier u5)

;; Data variables
(define-data-var recommendation-counter uint u0)
(define-data-var waste-record-counter uint u0)
(define-data-var total-waste-value uint u0)
(define-data-var carbon-footprint-saved uint u0)

;; Disposal recommendations based on item analysis
(define-map disposal-recommendations
    { recommendation-id: uint }
    {
        item-id: uint,
        days-until-expiry: uint,
        recommended-action: uint,
        financial-impact: uint,
        urgency-level: uint,
        markdown-percentage: uint,
        estimated-savings: uint,
        carbon-impact: uint,
        status: (string-ascii 20),
        created-at: uint,
        expires-at: uint
    }
)

;; Track financial impact of waste
(define-map waste-financial-tracking
    { item-id: uint }
    {
        total-disposed-value: uint,
        total-recovered-value: uint,
        disposal-count: uint,
        recovery-count: uint,
        net-loss: uint,
        last-disposal: uint,
        avg-disposal-value: uint
    }
)

;; Sustainability metrics for waste reduction
(define-map sustainability-metrics
    { item-id: uint }
    {
        carbon-footprint-disposed: uint,
        carbon-footprint-saved: uint,
        disposal-weight-kg: uint,
        recycling-potential: uint,
        environmental-score: uint,
        last-updated: uint
    }
)

;; Action history for learning and optimization
(define-map waste-action-history
    { action-id: uint }
    {
        item-id: uint,
        action-taken: uint,
        financial-result: uint,
        success-rate: uint,
        time-to-resolution: uint,
        user-rating: uint,
        timestamp: uint
    }
)

;; Alternative disposition options
(define-map disposition-options
    { item-id: uint, option-type: uint }
    {
        partner-name: (string-ascii 50),
        recovery-percentage: uint,
        processing-cost: uint,
        lead-time-blocks: uint,
        eligibility-criteria: (string-ascii 100),
        active: bool
    }
)

;; Core Functions

;; Analyze item and generate disposal recommendation
(define-public (generate-disposal-recommendation (item-id uint) (days-until-expiry uint) (current-price uint))
    (let 
        (
            (new-recommendation-id (+ (var-get recommendation-counter) u1))
            (urgency (calculate-urgency days-until-expiry))
            (recommended-action (determine-best-action item-id days-until-expiry current-price))
            (financial-impact (calculate-financial-impact item-id current-price recommended-action))
            (markdown-pct (calculate-optimal-markdown days-until-expiry))
            (estimated-savings (calculate-potential-savings item-id current-price recommended-action))
            (carbon-impact (calculate-carbon-impact item-id recommended-action))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (> current-price u0) err-invalid-parameters)
        
        (map-set disposal-recommendations
            { recommendation-id: new-recommendation-id }
            {
                item-id: item-id,
                days-until-expiry: days-until-expiry,
                recommended-action: recommended-action,
                financial-impact: financial-impact,
                urgency-level: urgency,
                markdown-percentage: markdown-pct,
                estimated-savings: estimated-savings,
                carbon-impact: carbon-impact,
                status: "pending",
                created-at: stacks-block-height,
                expires-at: (+ stacks-block-height days-until-expiry)
            }
        )
        (var-set recommendation-counter new-recommendation-id)
        (ok new-recommendation-id)
    )
)

;; Execute recommended action and track results
(define-public (execute-waste-action (recommendation-id uint) (user-rating uint))
    (let 
        (
            (recommendation (unwrap! (map-get? disposal-recommendations {recommendation-id: recommendation-id}) err-recommendation-not-found))
            (action-id (+ (var-get waste-record-counter) u1))
        )
        (asserts! (is-eq (get status recommendation) "pending") err-action-already-taken)
        (asserts! (and (>= user-rating u1) (<= user-rating u5)) err-invalid-parameters)
        
        ;; Update recommendation status
        (map-set disposal-recommendations
            { recommendation-id: recommendation-id }
            (merge recommendation { status: "executed" })
        )
        
        ;; Record action history
        (map-set waste-action-history
            { action-id: action-id }
            {
                item-id: (get item-id recommendation),
                action-taken: (get recommended-action recommendation),
                financial-result: (get estimated-savings recommendation),
                success-rate: (* user-rating u20),
                time-to-resolution: (- stacks-block-height (get created-at recommendation)),
                user-rating: user-rating,
                timestamp: stacks-block-height
            }
        )
        
        ;; Update financial tracking
        (unwrap! (update-waste-financial-tracking (get item-id recommendation) (get financial-impact recommendation) (get estimated-savings recommendation)) err-invalid-parameters)
        
        ;; Update sustainability metrics
        (unwrap! (update-sustainability-metrics (get item-id recommendation) (get carbon-impact recommendation) (get recommended-action recommendation)) err-invalid-parameters)
        
        (var-set waste-record-counter action-id)
        (ok action-id)
    )
)

;; Configure disposition options for different item types
(define-public (configure-disposition-option 
    (item-id uint) 
    (option-type uint) 
    (partner-name (string-ascii 50)) 
    (recovery-percentage uint) 
    (processing-cost uint) 
    (lead-time uint)
    (criteria (string-ascii 100))
)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (<= option-type u5) (> option-type u0)) err-invalid-parameters)
        (asserts! (<= recovery-percentage u100) err-invalid-parameters)
        
        (map-set disposition-options
            { item-id: item-id, option-type: option-type }
            {
                partner-name: partner-name,
                recovery-percentage: recovery-percentage,
                processing-cost: processing-cost,
                lead-time-blocks: lead-time,
                eligibility-criteria: criteria,
                active: true
            }
        )
        (ok true)
    )
)

;; Private helper functions

(define-private (calculate-urgency (days-until-expiry uint))
    (if (<= days-until-expiry u1)
        u5
        (if (<= days-until-expiry u3)
            u4
            (if (<= days-until-expiry u7)
                u3
                (if (<= days-until-expiry u14)
                    u2
                    u1
                )
            )
        )
    )
)

(define-private (determine-best-action (item-id uint) (days-until-expiry uint) (current-price uint))
    (if (<= days-until-expiry u1)
        action-dispose
        (if (<= days-until-expiry u3)
            action-donate
            (if (<= days-until-expiry u7)
                action-markdown
                (if (< current-price u1000)
                    action-repurpose
                    action-return-supplier
                )
            )
        )
    )
)

(define-private (calculate-financial-impact (item-id uint) (current-price uint) (action uint))
    (if (is-eq action action-dispose)
        current-price
        (if (is-eq action action-markdown)
            (/ (* current-price u50) u100)
            (if (is-eq action action-donate)
                (/ (* current-price u80) u100)
                (/ (* current-price u20) u100)
            )
        )
    )
)

(define-private (calculate-optimal-markdown (days-until-expiry uint))
    (if (<= days-until-expiry u1)
        u70
        (if (<= days-until-expiry u3)
            u50
            (if (<= days-until-expiry u7)
                u30
                u15
            )
        )
    )
)

(define-private (calculate-potential-savings (item-id uint) (current-price uint) (action uint))
    (let ((markdown-recovery (/ (* current-price (- u100 (calculate-optimal-markdown u3))) u100)))
        (if (is-eq action action-markdown)
            markdown-recovery
            (if (is-eq action action-donate)
                (/ (* current-price u25) u100)
                (if (is-eq action action-repurpose)
                    (/ (* current-price u40) u100)
                    u0
                )
            )
        )
    )
)

(define-private (calculate-carbon-impact (item-id uint) (action uint))
    (let ((base-carbon u100))
        (if (is-eq action action-dispose)
            base-carbon
            (if (is-eq action action-donate)
                u20
                (if (is-eq action action-repurpose)
                    u10
                    u50
                )
            )
        )
    )
)

(define-private (update-waste-financial-tracking (item-id uint) (financial-impact uint) (recovered-value uint))
    (let 
        (
            (current-tracking (default-to 
                { total-disposed-value: u0, total-recovered-value: u0, disposal-count: u0, recovery-count: u0, net-loss: u0, last-disposal: u0, avg-disposal-value: u0 }
                (map-get? waste-financial-tracking {item-id: item-id})
            ))
        )
        (map-set waste-financial-tracking
            { item-id: item-id }
            {
                total-disposed-value: (+ (get total-disposed-value current-tracking) financial-impact),
                total-recovered-value: (+ (get total-recovered-value current-tracking) recovered-value),
                disposal-count: (+ (get disposal-count current-tracking) u1),
                recovery-count: (if (> recovered-value u0) (+ (get recovery-count current-tracking) u1) (get recovery-count current-tracking)),
                net-loss: (+ (get net-loss current-tracking) (- financial-impact recovered-value)),
                last-disposal: stacks-block-height,
                avg-disposal-value: (/ (+ (get total-disposed-value current-tracking) financial-impact) (+ (get disposal-count current-tracking) u1))
            }
        )
        (var-set total-waste-value (+ (var-get total-waste-value) financial-impact))
        (ok true)
    )
)

(define-private (update-sustainability-metrics (item-id uint) (carbon-impact uint) (action uint))
    (let 
        (
            (current-metrics (default-to 
                { carbon-footprint-disposed: u0, carbon-footprint-saved: u0, disposal-weight-kg: u0, recycling-potential: u0, environmental-score: u0, last-updated: u0 }
                (map-get? sustainability-metrics {item-id: item-id})
            ))
            (carbon-saved (if (is-eq action action-dispose) u0 (- u100 carbon-impact)))
        )
        (map-set sustainability-metrics
            { item-id: item-id }
            {
                carbon-footprint-disposed: (+ (get carbon-footprint-disposed current-metrics) (if (is-eq action action-dispose) carbon-impact u0)),
                carbon-footprint-saved: (+ (get carbon-footprint-saved current-metrics) carbon-saved),
                disposal-weight-kg: (+ (get disposal-weight-kg current-metrics) u1),
                recycling-potential: (calculate-recycling-potential action),
                environmental-score: (calculate-environmental-score action carbon-saved),
                last-updated: stacks-block-height
            }
        )
        (var-set carbon-footprint-saved (+ (var-get carbon-footprint-saved) carbon-saved))
        (ok true)
    )
)

(define-private (calculate-recycling-potential (action uint))
    (if (is-eq action action-repurpose)
        u90
        (if (is-eq action action-donate)
            u70
            (if (is-eq action action-markdown)
                u60
                u10
            )
        )
    )
)

(define-private (calculate-environmental-score (action uint) (carbon-saved uint))
    (+ 
        (* action u20)
        (/ carbon-saved u2)
    )
)

;; Read-only functions

(define-read-only (get-disposal-recommendation (recommendation-id uint))
    (map-get? disposal-recommendations {recommendation-id: recommendation-id})
)

(define-read-only (get-waste-financial-impact (item-id uint))
    (map-get? waste-financial-tracking {item-id: item-id})
)

(define-read-only (get-sustainability-metrics (item-id uint))
    (map-get? sustainability-metrics {item-id: item-id})
)

(define-read-only (get-action-history (action-id uint))
    (map-get? waste-action-history {action-id: action-id})
)

(define-read-only (get-disposition-options (item-id uint) (option-type uint))
    (map-get? disposition-options {item-id: item-id, option-type: option-type})
)

(define-read-only (get-total-waste-impact)
    {
        total-waste-value: (var-get total-waste-value),
        total-carbon-saved: (var-get carbon-footprint-saved),
        total-recommendations: (var-get recommendation-counter),
        total-actions: (var-get waste-record-counter)
    }
)

(define-read-only (get-urgent-recommendations)
    (ok (var-get recommendation-counter))
)

(define-read-only (has-active-recommendation (item-id uint))
    (is-some (fold check-active-recommendation (list u1 u2 u3 u4 u5) none))
)

(define-private (check-active-recommendation (recommendation-id uint) (found (optional uint)))
    (if (is-some found)
        found
        (match (map-get? disposal-recommendations {recommendation-id: recommendation-id})
            recommendation 
            (if (is-eq (get status recommendation) "pending")
                (some recommendation-id)
                none
            )
            none
        )
    )
)

(define-read-only (calculate-waste-efficiency (item-id uint))
    (match (map-get? waste-financial-tracking {item-id: item-id})
        tracking-data 
        (let 
            (
                (total-value (+ (get total-disposed-value tracking-data) (get total-recovered-value tracking-data)))
                (recovery-rate (if (> total-value u0) 
                    (/ (* (get total-recovered-value tracking-data) u100) total-value) 
                    u0))
            )
            {
                recovery-rate: recovery-rate,
                disposal-frequency: (get disposal-count tracking-data),
                avg-loss-per-disposal: (get avg-disposal-value tracking-data),
                efficiency-score: (if (> recovery-rate u50) u100 (* recovery-rate u2))
            }
        )
        {
            recovery-rate: u0,
            disposal-frequency: u0,
            avg-loss-per-disposal: u0,
            efficiency-score: u0
        }
    )
)

(define-read-only (suggest-clearance-integration (item-id uint))
    (match (fold find-pending-recommendation (list u1 u2 u3 u4 u5) none)
        recommendation-id
        (match (map-get? disposal-recommendations {recommendation-id: recommendation-id})
            recommendation
            (if (is-eq (get recommended-action recommendation) action-markdown)
                (some {
                    markdown-percentage: (get markdown-percentage recommendation),
                    urgency: (get urgency-level recommendation),
                    estimated-savings: (get estimated-savings recommendation)
                })
                none
            )
            none
        )
        none
    )
)

(define-private (find-pending-recommendation (rec-id uint) (found (optional uint)))
    (if (is-some found)
        found
        (match (map-get? disposal-recommendations {recommendation-id: rec-id})
            recommendation
            (if (and 
                    (is-eq (get status recommendation) "pending")
                    (is-eq (get recommended-action recommendation) action-markdown)
                )
                (some rec-id)
                none
            )
            none
        )
    )
)