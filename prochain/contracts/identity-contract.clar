;; Decentralized Professional Network Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROFILE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-ENDORSED (err u102))
(define-constant ERR-INVALID-PRIVACY-LEVEL (err u103))
(define-constant ERR-CREDENTIAL-NOT-FOUND (err u104))

;; Privacy levels
(define-constant PRIVACY-PUBLIC u0)
(define-constant PRIVACY-CONNECTIONS u1)
(define-constant PRIVACY-PRIVATE u2)

;; Data structures
(define-map user-profiles
  principal
  {
    display-name: (string-ascii 50),
    bio: (string-ascii 500),
    profile-image: (string-ascii 200),
    privacy-level: uint,
    created-at: uint,
    is-verified: bool
  })

(define-map work-history
  { user: principal, work-id: uint }
  {
    company: (string-ascii 100),
    position: (string-ascii 100),
    start-date: uint,
    end-date: (optional uint),
    description: (string-ascii 500),
    privacy-level: uint
  })

(define-map credentials
  { user: principal, credential-id: uint }
  {
    title: (string-ascii 100),
    issuer: (string-ascii 100),
    issue-date: uint,
    expiry-date: (optional uint),
    verification-url: (string-ascii 200),
    privacy-level: uint,
    is-verified: bool
  })

(define-map endorsements
  { endorser: principal, endorsee: principal, skill: (string-ascii 50) }
  {
    message: (string-ascii 200),
    timestamp: uint,
    is-public: bool
  })

(define-map connections
  { user1: principal, user2: principal }
  {
    status: (string-ascii 20), ;; "pending", "accepted", "blocked"
    initiated-by: principal,
    timestamp: uint
  })

;; Counters for unique IDs
(define-data-var work-id-counter uint u0)
(define-data-var credential-id-counter uint u0)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Profile management functions
(define-public (create-profile (display-name (string-ascii 50)) (bio (string-ascii 500)) (profile-image (string-ascii 200)) (privacy-level uint))
  (begin
    (asserts! (<= privacy-level PRIVACY-PRIVATE) ERR-INVALID-PRIVACY-LEVEL)
    (ok (map-set user-profiles tx-sender {
      display-name: display-name,
      bio: bio,
      profile-image: profile-image,
      privacy-level: privacy-level,
      created-at: block-height,
      is-verified: false
    }))))

(define-public (update-profile (display-name (string-ascii 50)) (bio (string-ascii 500)) (profile-image (string-ascii 200)) (privacy-level uint))
  (begin
    (asserts! (<= privacy-level PRIVACY-PRIVATE) ERR-INVALID-PRIVACY-LEVEL)
    (asserts! (is-some (map-get? user-profiles tx-sender)) ERR-PROFILE-NOT-FOUND)
    (ok (map-set user-profiles tx-sender {
      display-name: display-name,
      bio: bio,
      profile-image: profile-image,
      privacy-level: privacy-level,
      created-at: (default-to block-height (get created-at (map-get? user-profiles tx-sender))),
      is-verified: (default-to false (get is-verified (map-get? user-profiles tx-sender)))
    }))))

;; Work history functions
(define-public (add-work-experience (company (string-ascii 100)) (position (string-ascii 100)) (start-date uint) (end-date (optional uint)) (description (string-ascii 500)) (privacy-level uint))
  (let ((work-id (+ (var-get work-id-counter) u1)))
    (begin
      (asserts! (<= privacy-level PRIVACY-PRIVATE) ERR-INVALID-PRIVACY-LEVEL)
      (asserts! (is-some (map-get? user-profiles tx-sender)) ERR-PROFILE-NOT-FOUND)
      (var-set work-id-counter work-id)
      (ok (map-set work-history { user: tx-sender, work-id: work-id } {
        company: company,
        position: position,
        start-date: start-date,
        end-date: end-date,
        description: description,
        privacy-level: privacy-level
      })))))

;; Credential functions
(define-public (add-credential (title (string-ascii 100)) (issuer (string-ascii 100)) (issue-date uint) (expiry-date (optional uint)) (verification-url (string-ascii 200)) (privacy-level uint))
  (let ((credential-id (+ (var-get credential-id-counter) u1)))
    (begin
      (asserts! (<= privacy-level PRIVACY-PRIVATE) ERR-INVALID-PRIVACY-LEVEL)
      (asserts! (is-some (map-get? user-profiles tx-sender)) ERR-PROFILE-NOT-FOUND)
      (var-set credential-id-counter credential-id)
      (ok (map-set credentials { user: tx-sender, credential-id: credential-id } {
        title: title,
        issuer: issuer,
        issue-date: issue-date,
        expiry-date: expiry-date,
        verification-url: verification-url,
        privacy-level: privacy-level,
        is-verified: false
      })))))

(define-public (verify-credential (user principal) (credential-id uint))
  (let ((credential (map-get? credentials { user: user, credential-id: credential-id })))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
      (asserts! (is-some credential) ERR-CREDENTIAL-NOT-FOUND)
      (ok (map-set credentials { user: user, credential-id: credential-id }
        (merge (unwrap-panic credential) { is-verified: true }))))))

;; Endorsement functions
(define-public (endorse-skill (endorsee principal) (skill (string-ascii 50)) (message (string-ascii 200)) (is-public bool))
  (begin
    (asserts! (is-some (map-get? user-profiles tx-sender)) ERR-PROFILE-NOT-FOUND)
    (asserts! (is-some (map-get? user-profiles endorsee)) ERR-PROFILE-NOT-FOUND)
    (asserts! (is-none (map-get? endorsements { endorser: tx-sender, endorsee: endorsee, skill: skill })) ERR-ALREADY-ENDORSED)
    (ok (map-set endorsements { endorser: tx-sender, endorsee: endorsee, skill: skill } {
      message: message,
      timestamp: block-height,
      is-public: is-public
    }))))

;; Connection functions
(define-public (send-connection-request (to-user principal))
  (begin
    (asserts! (is-some (map-get? user-profiles tx-sender)) ERR-PROFILE-NOT-FOUND)
    (asserts! (is-some (map-get? user-profiles to-user)) ERR-PROFILE-NOT-FOUND)
    (ok (map-set connections { user1: tx-sender, user2: to-user } {
      status: "pending",
      initiated-by: tx-sender,
      timestamp: block-height
    }))))

(define-public (accept-connection (from-user principal))
  (let ((connection (map-get? connections { user1: from-user, user2: tx-sender })))
    (begin
      (asserts! (is-some connection) ERR-PROFILE-NOT-FOUND)
      (asserts! (is-eq (get status (unwrap-panic connection)) "pending") ERR-NOT-AUTHORIZED)
      (ok (map-set connections { user1: from-user, user2: tx-sender }
        (merge (unwrap-panic connection) { status: "accepted" }))))))

;; Read-only functions with privacy controls
(define-read-only (get-profile (user principal))
  (let ((profile (map-get? user-profiles user)))
    (if (is-some profile)
      (let ((profile-data (unwrap-panic profile)))
        (if (or (is-eq (get privacy-level profile-data) PRIVACY-PUBLIC)
                (is-eq user tx-sender)
                (is-connected user tx-sender))
          profile
          none))
      none)))

(define-read-only (get-work-history (user principal) (work-id uint))
  (let ((work (map-get? work-history { user: user, work-id: work-id })))
    (if (is-some work)
      (let ((work-data (unwrap-panic work)))
        (if (can-view-data user (get privacy-level work-data))
          work
          none))
      none)))

(define-read-only (get-credential (user principal) (credential-id uint))
  (let ((credential (map-get? credentials { user: user, credential-id: credential-id })))
    (if (is-some credential)
      (let ((credential-data (unwrap-panic credential)))
        (if (can-view-data user (get privacy-level credential-data))
          credential
          none))
      none)))

(define-read-only (get-endorsement (endorser principal) (endorsee principal) (skill (string-ascii 50)))
  (let ((endorsement (map-get? endorsements { endorser: endorser, endorsee: endorsee, skill: skill })))
    (if (is-some endorsement)
      (let ((endorsement-data (unwrap-panic endorsement)))
        (if (or (get is-public endorsement-data)
                (is-eq endorsee tx-sender)
                (is-connected endorsee tx-sender))
          endorsement
          none))
      none)))

;; Helper functions
(define-read-only (is-connected (user1 principal) (user2 principal))
  (or (is-eq (get status (default-to { status: "none", initiated-by: user1, timestamp: u0 } 
                          (map-get? connections { user1: user1, user2: user2 }))) "accepted")
      (is-eq (get status (default-to { status: "none", initiated-by: user2, timestamp: u0 } 
                          (map-get? connections { user1: user2, user2: user1 }))) "accepted")))

(define-read-only (can-view-data (data-owner principal) (privacy-level uint))
  (or (is-eq privacy-level PRIVACY-PUBLIC)
      (is-eq data-owner tx-sender)
      (and (is-eq privacy-level PRIVACY-CONNECTIONS) (is-connected data-owner tx-sender))))

;; Admin functions
(define-public (verify-profile (user principal))
  (let ((profile (map-get? user-profiles user)))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
      (asserts! (is-some profile) ERR-PROFILE-NOT-FOUND)
      (ok (map-set user-profiles user
        (merge (unwrap-panic profile) { is-verified: true }))))))

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))))