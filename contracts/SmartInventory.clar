;; SmartInventory
;; Supply chain inventory management system

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-invalid-quantity (err u101))
(define-constant err-item-exists (err u102))
(define-constant err-item-not-found (err u103))
(define-constant err-insufficient-stock (err u104))
(define-constant err-invalid-supplier (err u105))

;; Data Variables
(define-data-var total-items uint u0)
(define-data-var total-suppliers uint u0)

;; Data Maps
(define-map inventory
    { item-id: uint }
    {
        name: (string-ascii 50),
        quantity: uint,
        min-threshold: uint,
        supplier-id: uint,
        price: uint,
        last-updated: uint
    }
)

(define-map suppliers
    { supplier-id: uint }
    {
        name: (string-ascii 50),
        verified: bool,
        rating: uint,
        active: bool
    }
)

(define-map orders
    { order-id: uint }
    {
        item-id: uint,
        quantity: uint,
        status: (string-ascii 20),
        timestamp: uint
    }
)

;; Public Functions

;; Add new inventory item
(define-public (add-item (name (string-ascii 50)) (quantity uint) (min-threshold uint) (supplier-id uint) (price uint))
    (let
        ((new-item-id (+ (var-get total-items) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-supplier-valid supplier-id) err-invalid-supplier)
        (asserts! (> quantity u0) err-invalid-quantity)
        (asserts! (map-insert inventory
            { item-id: new-item-id }
            {
                name: name,
                quantity: quantity,
                min-threshold: min-threshold,
                supplier-id: supplier-id,
                price: price,
                last-updated: stacks-block-height
            }
        ) err-item-exists)
        (var-set total-items new-item-id)
        (ok new-item-id)
    )
)

;; Update inventory quantity
(define-public (update-quantity (item-id uint) (new-quantity uint))
    (let ((item (unwrap! (map-get? inventory {item-id: item-id}) err-item-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (>= new-quantity u0) err-invalid-quantity)
        (map-set inventory
            {item-id: item-id}
            (merge item {
                quantity: new-quantity,
                last-updated: stacks-block-height
            })
        )
        (ok true)
    )
)

;; Add new supplier
(define-public (add-supplier (name (string-ascii 50)))
    (let
        ((new-supplier-id (+ (var-get total-suppliers) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (map-insert suppliers
            { supplier-id: new-supplier-id }
            {
                name: name,
                verified: false,
                rating: u0,
                active: true
            }
        ) err-item-exists)
        (var-set total-suppliers new-supplier-id)
        (ok new-supplier-id)
    )
)

;; Create order
(define-public (create-order (item-id uint) (quantity uint))
    (let
        ((item (unwrap! (map-get? inventory {item-id: item-id}) err-item-not-found)))
        (asserts! (>= (get quantity item) quantity) err-insufficient-stock)
        (asserts! (map-insert orders
            { order-id: (+ stacks-block-height u1) }
            {
                item-id: item-id,
                quantity: quantity,
                status: "pending",
                timestamp: stacks-block-height
            }
        ) err-item-exists)
        (try! (update-quantity item-id (- (get quantity item) quantity)))
        (ok true)
    )
)

;; Read Only Functions

;; Check if supplier exists and is active
(define-read-only (is-supplier-valid (supplier-id uint))
    (match (map-get? suppliers {supplier-id: supplier-id})
        supplier (get active supplier)
        false
    )
)

;; Get item details
(define-read-only (get-item (item-id uint))
    (map-get? inventory {item-id: item-id})
)

;; Get supplier details
(define-read-only (get-supplier (supplier-id uint))
    (map-get? suppliers {supplier-id: supplier-id})
)

;; Get order details
(define-read-only (get-order (order-id uint))
    (map-get? orders {order-id: order-id})
)

;; Check if item needs restock
(define-read-only (needs-restock (item-id uint))
    (match (map-get? inventory {item-id: item-id})
        item (< (get quantity item) (get min-threshold item))
        false
    )
)

(define-map batch-orders 
    { batch-id: uint }
    {
        order-ids: (list 50 uint),
        status: (string-ascii 20),
        timestamp: uint
    }
)

(define-data-var batch-counter uint u0)

(define-public (create-batch-order (order-list (list 50 uint)))
    (let ((new-batch-id (+ (var-get batch-counter) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set batch-orders
            { batch-id: new-batch-id }
            {
                order-ids: order-list,
                status: "pending",
                timestamp: stacks-block-height
            }
        )
        (var-set batch-counter new-batch-id)
        (ok new-batch-id)
    )
)


(define-map categories
    { category-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 100)
    }
)

(define-data-var category-counter uint u0)

(define-public (add-category (name (string-ascii 50)) (description (string-ascii 100)))
    (let ((new-category-id (+ (var-get category-counter) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set categories
            { category-id: new-category-id }
            {
                name: name,
                description: description
            }
        )
        (var-set category-counter new-category-id)
        (ok new-category-id)
    )
)


(define-map price-history
    { item-id: uint, timestamp: uint }
    { price: uint }
)

(define-public (update-item-price (item-id uint) (new-price uint))
    (let ((item (unwrap! (map-get? inventory {item-id: item-id}) err-item-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set price-history
            { item-id: item-id, timestamp: stacks-block-height }
            { price: new-price }
        )
        (map-set inventory
            {item-id: item-id}
            (merge item { price: new-price })
        )
        (ok true)
    )
)

(define-map quality-checks
    { check-id: uint }
    {
        item-id: uint,
        inspector: principal,
        status: (string-ascii 20),
        notes: (string-ascii 100),
        timestamp: uint
    }
)

(define-data-var quality-check-counter uint u0)

(define-public (record-quality-check (item-id uint) (status (string-ascii 20)) (notes (string-ascii 100)))
    (let ((new-check-id (+ (var-get quality-check-counter) u1)))
        (map-set quality-checks
            { check-id: new-check-id }
            {
                item-id: item-id,
                inspector: tx-sender,
                status: status,
                notes: notes,
                timestamp: stacks-block-height
            }
        )
        (var-set quality-check-counter new-check-id)
        (ok new-check-id)
    )
)


(define-map returns
    { return-id: uint }
    {
        order-id: uint,
        reason: (string-ascii 50),
        quantity: uint,
        status: (string-ascii 20),
        timestamp: uint
    }
)

(define-data-var return-counter uint u0)

(define-public (process-return (order-id uint) (quantity uint) (reason (string-ascii 50)))
    (let 
        (
            (new-return-id (+ (var-get return-counter) u1))
            (order (unwrap! (get-order order-id) err-item-not-found))
        )
        (map-set returns
            { return-id: new-return-id }
            {
                order-id: order-id,
                reason: reason,
                quantity: quantity,
                status: "pending",
                timestamp: stacks-block-height
            }
        )
        (var-set return-counter new-return-id)
        (ok new-return-id)
    )
)


(define-map alerts
    { alert-id: uint }
    {
        item-id: uint,
        alert-type: (string-ascii 20),
        message: (string-ascii 100),
        status: (string-ascii 20),
        timestamp: uint
    }
)

(define-data-var alert-counter uint u0)

(define-public (create-alert (item-id uint) (alert-type (string-ascii 20)) (message (string-ascii 100)))
    (let ((new-alert-id (+ (var-get alert-counter) u1)))
        (map-set alerts
            { alert-id: new-alert-id }
            {
                item-id: item-id,
                alert-type: alert-type,
                message: message,
                status: "active",
                timestamp: stacks-block-height
            }
        )
        (var-set alert-counter new-alert-id)
        (ok new-alert-id)
    )
)


(define-map bulk-operations
    { operation-id: uint }
    {
        operation-type: (string-ascii 20),
        items: (list 50 uint),
        status: (string-ascii 20),
        timestamp: uint
    }
)

(define-data-var operation-counter uint u0)

(define-public (create-bulk-operation (op-type (string-ascii 20)) (item-list (list 50 uint)))
    (let ((new-operation-id (+ (var-get operation-counter) u1)))
        (map-set bulk-operations
            { operation-id: new-operation-id }
            {
                operation-type: op-type,
                items: item-list,
                status: "pending",
                timestamp: stacks-block-height
            }
        )
        (var-set operation-counter new-operation-id)
        (ok new-operation-id)
    )
)


(define-map bundles
    { bundle-id: uint }
    {
        name: (string-ascii 50),
        items: (list 10 uint),
        quantities: (list 10 uint),
        bundle-price: uint,
        active: bool
    }
)

(define-data-var bundle-counter uint u0)

(define-public (create-bundle (name (string-ascii 50)) (items (list 10 uint)) (quantities (list 10 uint)) (bundle-price uint))
    (let ((new-bundle-id (+ (var-get bundle-counter) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set bundles
            { bundle-id: new-bundle-id }
            {
                name: name,
                items: items,
                quantities: quantities,
                bundle-price: bundle-price,
                active: true
            }
        )
        (var-set bundle-counter new-bundle-id)
        (ok new-bundle-id)
    )
)




(define-map inventory-metrics
    { item-id: uint }
    {
        total-sales: uint,
        restock-frequency: uint,
        avg-order-size: uint,
        last-calculated: uint
    }
)

(define-public (update-metrics (item-id uint))
    (let 
        (
            (current-metrics (default-to 
                { total-sales: u0, restock-frequency: u0, avg-order-size: u0, last-calculated: u0 }
                (map-get? inventory-metrics {item-id: item-id})))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set inventory-metrics
            { item-id: item-id }
            {
                total-sales: (+ (get total-sales current-metrics) u1),
                restock-frequency: (calculate-restock-frequency item-id),
                avg-order-size: (calculate-avg-order-size item-id),
                last-calculated: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-read-only (calculate-restock-frequency (item-id uint))
    (let ((item (unwrap! (get-item item-id) u0)))
        (if (needs-restock item-id)
            (+ u1 u0)
            u0
        )
    )
)

(define-read-only (calculate-avg-order-size (item-id uint))
    (default-to u0 (some u10))
)


(define-map reorder-settings
    { item-id: uint }
    {
        lead-time-days: uint,
        max-stock: uint,
        auto-reorder: bool,
        last-reorder: uint,
        reorder-quantity: uint
    }
)

(define-map reorder-queue
    { queue-id: uint }
    {
        item-id: uint,
        quantity: uint,
        supplier-id: uint,
        status: (string-ascii 20),
        created-at: uint
    }
)

(define-data-var reorder-queue-counter uint u0)

(define-public (configure-reorder-settings 
    (item-id uint) 
    (lead-time uint) 
    (max-stock uint) 
    (auto-reorder bool)
    (reorder-quantity uint)
)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (map-set reorder-settings
            { item-id: item-id }
            {
                lead-time-days: lead-time,
                max-stock: max-stock,
                auto-reorder: auto-reorder,
                last-reorder: u0,
                reorder-quantity: reorder-quantity
            }
        ) err-item-exists)
        (ok true)
    )
)

(define-public (check-and-reorder (item-id uint))
    (let 
        (
            (item (unwrap! (get-item item-id) err-item-not-found))
            (settings (unwrap! (map-get? reorder-settings {item-id: item-id}) err-item-not-found))
            (new-queue-id (+ (var-get reorder-queue-counter) u1))
        )
        (asserts! (get auto-reorder settings) err-not-authorized)
        (asserts! (needs-restock item-id) err-invalid-quantity)
        (map-set reorder-queue
            { queue-id: new-queue-id }
            {
                item-id: item-id,
                quantity: (get reorder-quantity settings),
                supplier-id: (get supplier-id item),
                status: "pending",
                created-at: stacks-block-height
            }
        )
        (var-set reorder-queue-counter new-queue-id)
        (ok new-queue-id)
    )
)

(define-read-only (get-pending-reorders)
    (ok (var-get reorder-queue-counter))
)