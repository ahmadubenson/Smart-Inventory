;; Dynamic Pricing Engine with Market Intelligence
;; Automatically adjusts prices based on supply, demand, and market conditions

;; Constants for pricing engine
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-invalid-price (err u401))
(define-constant err-invalid-parameters (err u402))
(define-constant err-rule-not-found (err u403))
(define-constant err-invalid-demand-data (err u404))

;; Pricing strategy constants
(define-constant strategy-supply-based u1)
(define-constant strategy-demand-based u2)
(define-constant strategy-seasonal u3)
(define-constant strategy-clearance u4)
(define-constant strategy-competitor u5)

;; Data variables for pricing engine
(define-data-var pricing-rule-counter uint u0)
(define-data-var demand-record-counter uint u0)
(define-data-var market-adjustment-factor uint u100) ;; Base 100 = 1.0x

;; Core pricing rules for each item
(define-map pricing-rules
    { item-id: uint }
    {
        base-price: uint,
        min-price: uint,
        max-price: uint,
        strategy: uint,
        markup-percentage: uint,
        auto-adjust: bool,
        last-updated: uint,
        price-volatility: uint  ;; How much price can change (percentage)
    }
)

;; Track demand patterns for intelligent pricing
(define-map demand-analytics
    { item-id: uint, period: uint }  ;; period = week number
    {
        total-orders: uint,
        total-quantity: uint,
        avg-order-size: uint,
        peak-demand-day: uint,
        demand-trend: (string-ascii 20),  ;; "increasing", "stable", "decreasing"
        velocity-score: uint  ;; Sales velocity 1-100
    }
)

;; Market competitor pricing data
(define-map competitor-prices
    { item-id: uint, competitor-id: uint }
    {
        competitor-name: (string-ascii 50),
        price: uint,
        last-updated: uint,
        market-share: uint,  ;; percentage
        reliability-score: uint  ;; 1-100
    }
)

;; Seasonal pricing modifiers
(define-map seasonal-adjustments
    { item-id: uint, season: uint }  ;; season: 1=spring, 2=summer, 3=fall, 4=winter
    {
        price-modifier: uint,  ;; percentage adjustment (100 = no change)
        demand-multiplier: uint,
        active: bool,
        historical-data: uint  ;; years of data supporting this adjustment
    }
)

;; Price change history for analysis
(define-map price-history
    { item-id: uint, timestamp: uint }
    {
        old-price: uint,
        new-price: uint,
        change-reason: (string-ascii 50),
        strategy-used: uint,
        market-response: uint  ;; 1-10 scale of positive response
    }
)

;; Real-time inventory impact on pricing
(define-map inventory-pricing-impact
    { item-id: uint }
    {
        stock-level: uint,
        optimal-stock: uint,
        overstock-threshold: uint,
        shortage-threshold: uint,
        price-elasticity: uint,  ;; how sensitive demand is to price changes
        turnover-rate: uint
    }
)

;; Create or update pricing rules for an item
(define-public (set-pricing-rule 
    (item-id uint) 
    (base-price uint) 
    (min-price uint) 
    (max-price uint) 
    (strategy uint) 
    (markup-percentage uint) 
    (auto-adjust bool)
    (price-volatility uint)
)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (> base-price u0) (>= base-price min-price) (<= base-price max-price)) err-invalid-price)
        (asserts! (<= strategy u5) err-invalid-parameters)
        (asserts! (<= price-volatility u50) err-invalid-parameters)  ;; Max 50% volatility
        
        (map-set pricing-rules
            { item-id: item-id }
            {
                base-price: base-price,
                min-price: min-price,
                max-price: max-price,
                strategy: strategy,
                markup-percentage: markup-percentage,
                auto-adjust: auto-adjust,
                last-updated: stacks-block-height,
                price-volatility: price-volatility
            }
        )
        (ok true)
    )
)

;; Record demand data for pricing intelligence
(define-public (record-demand-data 
    (item-id uint) 
    (period uint) 
    (orders uint) 
    (quantity uint) 
    (avg-size uint) 
    (peak-day uint)
    (trend (string-ascii 20))
    (velocity uint)
)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (> orders u0) (> quantity u0) (<= velocity u100)) err-invalid-demand-data)
        
        (map-set demand-analytics
            { item-id: item-id, period: period }
            {
                total-orders: orders,
                total-quantity: quantity,
                avg-order-size: avg-size,
                peak-demand-day: peak-day,
                demand-trend: trend,
                velocity-score: velocity
            }
        )
        (ok true)
    )
)

;; Update competitor pricing data
(define-public (update-competitor-price 
    (item-id uint) 
    (competitor-id uint) 
    (name (string-ascii 50)) 
    (price uint) 
    (market-share uint) 
    (reliability uint)
)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (> price u0) (<= market-share u100) (<= reliability u100)) err-invalid-parameters)
        
        (map-set competitor-prices
            { item-id: item-id, competitor-id: competitor-id }
            {
                competitor-name: name,
                price: price,
                last-updated: stacks-block-height,
                market-share: market-share,
                reliability-score: reliability
            }
        )
        (ok true)
    )
)

;; Set seasonal pricing adjustments
(define-public (configure-seasonal-pricing 
    (item-id uint) 
    (season uint) 
    (price-modifier uint) 
    (demand-multiplier uint) 
    (historical-years uint)
)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (<= season u4) (> season u0)) err-invalid-parameters)
        (asserts! (and (>= price-modifier u50) (<= price-modifier u200)) err-invalid-parameters)  ;; 50%-200% range
        
        (map-set seasonal-adjustments
            { item-id: item-id, season: season }
            {
                price-modifier: price-modifier,
                demand-multiplier: demand-multiplier,
                active: true,
                historical-data: historical-years
            }
        )
        (ok true)
    )
)

;; Calculate dynamic price based on multiple factors
(define-public (calculate-dynamic-price (item-id uint) (current-stock uint) (current-season uint))
    (let 
        (
            (pricing-rule (unwrap! (map-get? pricing-rules {item-id: item-id}) err-rule-not-found))
            (base-price (get base-price pricing-rule))
            (strategy (get strategy pricing-rule))
            (volatility (get price-volatility pricing-rule))
            (min-price (get min-price pricing-rule))
            (max-price (get max-price pricing-rule))
        )
        (asserts! (get auto-adjust pricing-rule) err-not-authorized)
        
        ;; Calculate price adjustments based on strategy
        (let 
            (
                (supply-adjustment (calculate-supply-adjustment item-id current-stock))
                (demand-adjustment (calculate-demand-adjustment item-id))
                (seasonal-adjustment (calculate-seasonal-adjustment item-id current-season))
                (competitor-adjustment (calculate-competitor-adjustment item-id))
                (combined-modifier (combine-price-factors supply-adjustment demand-adjustment seasonal-adjustment competitor-adjustment))
                (new-price (apply-price-modifier base-price combined-modifier volatility))
                (final-price (bound-price new-price min-price max-price))
            )
            ;; Record the price change
            (map-set price-history
                { item-id: item-id, timestamp: stacks-block-height }
                {
                    old-price: base-price,
                    new-price: final-price,
                    change-reason: "dynamic-calculation",
                    strategy-used: strategy,
                    market-response: u5  ;; Default neutral response
                }
            )
            (ok final-price)
        )
    )
)

;; Update inventory impact factors for pricing
(define-public (set-inventory-pricing-impact 
    (item-id uint) 
    (stock-level uint) 
    (optimal-stock uint) 
    (overstock-threshold uint) 
    (shortage-threshold uint)
    (price-elasticity uint)
    (turnover-rate uint)
)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (and (> optimal-stock u0) (< shortage-threshold optimal-stock) (> overstock-threshold optimal-stock)) err-invalid-parameters)
        
        (map-set inventory-pricing-impact
            { item-id: item-id }
            {
                stock-level: stock-level,
                optimal-stock: optimal-stock,
                overstock-threshold: overstock-threshold,
                shortage-threshold: shortage-threshold,
                price-elasticity: price-elasticity,
                turnover-rate: turnover-rate
            }
        )
        (ok true)
    )
)

;; Calculate supply-based price adjustment
(define-private (calculate-supply-adjustment (item-id uint) (current-stock uint))
    (match (map-get? inventory-pricing-impact {item-id: item-id})
        impact-data 
        (let 
            (
                (optimal (get optimal-stock impact-data))
                (overstock (get overstock-threshold impact-data))
                (shortage (get shortage-threshold impact-data))
            )
            (if (<= current-stock shortage)
                u120  ;; 20% price increase for shortage
                (if (>= current-stock overstock)
                    u85   ;; 15% price decrease for overstock
                    u100  ;; No adjustment for optimal levels
                )
            )
        )
        u100  ;; Default no adjustment
    )
)

;; Calculate demand-based price adjustment
(define-private (calculate-demand-adjustment (item-id uint))
    (let ((current-period (/ stacks-block-height u1000)))  ;; Simplified period calculation
        (match (map-get? demand-analytics {item-id: item-id, period: current-period})
            demand-data 
            (let ((velocity (get velocity-score demand-data)))
                (if (>= velocity u80)
                    u110  ;; 10% increase for high demand
                    (if (<= velocity u30)
                        u90   ;; 10% decrease for low demand
                        u100  ;; No adjustment for normal demand
                    )
                )
            )
            u100  ;; Default no adjustment
        )
    )
)

;; Calculate seasonal price adjustment
(define-private (calculate-seasonal-adjustment (item-id uint) (season uint))
    (match (map-get? seasonal-adjustments {item-id: item-id, season: season})
        seasonal-data 
        (if (get active seasonal-data)
            (get price-modifier seasonal-data)
            u100
        )
        u100  ;; Default no adjustment
    )
)

;; Calculate competitor-based price adjustment
(define-private (calculate-competitor-adjustment (item-id uint))
    ;; Simplified calculation - in practice would analyze multiple competitors
    u100  ;; Placeholder for competitor analysis
)

;; Combine multiple pricing factors
(define-private (combine-price-factors (supply uint) (demand uint) (seasonal uint) (competitor uint))
    (/ (* (* (* supply demand) seasonal) competitor) u1000000)  ;; Normalize the multiplication
)

;; Apply price modifier with volatility constraints
(define-private (apply-price-modifier (base-price uint) (modifier uint) (max-volatility uint))
    (let 
        (
            (adjusted-price (/ (* base-price modifier) u100))
            (max-change (/ (* base-price max-volatility) u100))
            (price-change (if (> adjusted-price base-price) 
                            (- adjusted-price base-price) 
                            (- base-price adjusted-price)))
        )
        (if (<= price-change max-change)
            adjusted-price
            (if (> adjusted-price base-price)
                (+ base-price max-change)
                (- base-price max-change)
            )
        )
    )
)

;; Ensure price stays within defined bounds
(define-private (bound-price (price uint) (min-price uint) (max-price uint))
    (if (< price min-price)
        min-price
        (if (> price max-price)
            max-price
            price
        )
    )
)

;; Read-only functions for pricing analysis

(define-read-only (get-pricing-rule (item-id uint))
    (map-get? pricing-rules {item-id: item-id})
)

(define-read-only (get-demand-data (item-id uint) (period uint))
    (map-get? demand-analytics {item-id: item-id, period: period})
)

(define-read-only (get-competitor-price (item-id uint) (competitor-id uint))
    (map-get? competitor-prices {item-id: item-id, competitor-id: competitor-id})
)

(define-read-only (get-seasonal-adjustment (item-id uint) (season uint))
    (map-get? seasonal-adjustments {item-id: item-id, season: season})
)

(define-read-only (get-price-history (item-id uint) (timestamp uint))
    (map-get? price-history {item-id: item-id, timestamp: timestamp})
)

(define-read-only (get-inventory-impact (item-id uint))
    (map-get? inventory-pricing-impact {item-id: item-id})
)

;; Check if item qualifies for clearance pricing
(define-read-only (needs-clearance-pricing (item-id uint))
    (match (map-get? inventory-pricing-impact {item-id: item-id})
        impact-data 
        (let 
            (
                (stock (get stock-level impact-data))
                (overstock (get overstock-threshold impact-data))
                (turnover (get turnover-rate impact-data))
            )
            (and (>= stock overstock) (<= turnover u20))  ;; High stock + low turnover
        )
        false
    )
)

;; Get recommended price for immediate sale
(define-read-only (get-clearance-price (item-id uint))
    (match (map-get? pricing-rules {item-id: item-id})
        rule-data 
        (let 
            (
                (base-price (get base-price rule-data))
                (min-price (get min-price rule-data))
                ;; Aggressive 30% discount for clearance
                (clearance-price (/ (* base-price u70) u100))
            )
            (if (>= clearance-price min-price)
                clearance-price
                min-price
            )
        )
        u0
    )
)
