;;-----------------------------------------------------------------------------
;; Digital Content Management System
;; A decentralized solution for managing digital assets and sharing permissions
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Global System Configuration
;;-----------------------------------------------------------------------------

;; System administrator declaration
(define-constant PLATFORM_ADMIN tx-sender)

;;-----------------------------------------------------------------------------
;; Response Codes for Operation Results
;;-----------------------------------------------------------------------------

(define-constant RESP_ASSET_MISSING (err u301))        ;; Asset record not found
(define-constant RESP_ASSET_EXISTS (err u302))         ;; Asset already registered
(define-constant RESP_TITLE_INVALID (err u303))        ;; Asset title validation failed
(define-constant RESP_FILESIZE_INVALID (err u304))     ;; Asset size limit exceeded
(define-constant RESP_PERMISSION_DENIED (err u305))    ;; Unauthorized operation attempt
(define-constant RESP_GENRE_INVALID (err u306))        ;; Invalid content category
(define-constant RESP_OPERATION_RESTRICTED (err u307)) ;; Operation not allowed
(define-constant RESP_NO_ACCESS (err u308))            ;; User lacks required access 
(define-constant RESP_INVALID_SHARING (err u309))      ;; Invalid access grant parameters
(define-constant RESP_INVALID_USER (err u310))         ;; User address validation failed
(define-constant RESP_BOOKMARK_EXISTS (err u311))      ;; Asset already bookmarked
(define-constant RESP_NOT_BOOKMARKED (err u312))       ;; Asset not in bookmarks

;;-----------------------------------------------------------------------------
;; System State Storage
;;-----------------------------------------------------------------------------

;; Master counter for asset tracking
(define-data-var asset-tracker uint u0)

;; Primary asset registry
(define-map asset-catalog
  { asset-id: uint }
  {
    title: (string-ascii 64),           ;; Asset title
    publisher: principal,               ;; Asset publisher identity
    storage-size: uint,                 ;; File storage requirements
    publish-time: uint,                 ;; Publication timestamp (block height)
    genre: (string-ascii 32),           ;; Content classification
    synopsis: (string-ascii 128),       ;; Brief content description
    topics: (list 10 (string-ascii 32)) ;; Content topic identifiers
  }
)


;; User permission registry
(define-map permission-ledger
  { asset-id: uint, viewer: principal }
  { 
    authorized: bool,                   ;; Access permission status
    authorizer: principal,              ;; Identity that granted access
    timestamp: uint                     ;; Permission grant timestamp
  }
)

;; User preference tracking
(define-map bookmark-registry
  { viewer: principal, asset-id: uint }
  {
    created: uint,                      ;; Bookmark creation timestamp
    modified: uint                      ;; Last modification timestamp
  }
)

;;-----------------------------------------------------------------------------
;; Internal Utility Functions
;;-----------------------------------------------------------------------------

;; Verify asset existence
(define-private (asset-exists? (asset-id uint))
  (is-some (map-get? asset-catalog { asset-id: asset-id }))
)

;; Verify ownership rights
(define-private (is-publisher? (asset-id uint) (user principal))
  (match (map-get? asset-catalog { asset-id: asset-id })
    asset-data (is-eq (get publisher asset-data) user)
    false
  )
)

;; Validate user principal
(define-private (is-valid-user? (user principal))
  (not (is-eq user 'ST000000000000000000002AMW42H))
)

;; Check viewer access rights
(define-private (has-viewing-rights? (asset-id uint) (user principal))
  (match (map-get? permission-ledger { asset-id: asset-id, viewer: user })
    permission-data (get authorized permission-data)
    false
  )
)

;; Check bookmark status
(define-private (is-bookmarked? (asset-id uint) (user principal))
  (is-some (map-get? bookmark-registry { viewer: user, asset-id: asset-id }))
)

;; Retrieve asset storage requirements
(define-private (get-storage-requirement (asset-id uint))
  (default-to u0 
    (get storage-size 
      (map-get? asset-catalog { asset-id: asset-id })
    )
  )
)

;;-----------------------------------------------------------------------------
;; Topic Validation Functions
;;-----------------------------------------------------------------------------

;; Validate individual topic format
(define-private (is-valid-topic? (topic (string-ascii 32)))
  (and 
    (> (len topic) u0)
    (< (len topic) u33)
  )
)

;; Validate complete topic collection
(define-private (are-topics-valid? (topics (list 10 (string-ascii 32))))
  (and
    (> (len topics) u0)
    (<= (len topics) u10)
    (is-eq (len (filter is-valid-topic? topics)) (len topics))
  )
)

;;-----------------------------------------------------------------------------
;; Asset Management Functions
;;-----------------------------------------------------------------------------

;; Register new digital asset
(define-public (publish-asset (title (string-ascii 64)) (filesize uint) (genre (string-ascii 32)) (synopsis (string-ascii 128)) (topics (list 10 (string-ascii 32))))
  (let
    (
      (next-id (+ (var-get asset-tracker) u1))
    )
    ;; Input validation
    (asserts! (> (len title) u0) RESP_TITLE_INVALID)
    (asserts! (< (len title) u65) RESP_TITLE_INVALID)
    (asserts! (> filesize u0) RESP_FILESIZE_INVALID)
    (asserts! (< filesize u1000000000) RESP_FILESIZE_INVALID)
    (asserts! (> (len genre) u0) RESP_GENRE_INVALID)
    (asserts! (< (len genre) u33) RESP_GENRE_INVALID)
    (asserts! (> (len synopsis) u0) RESP_TITLE_INVALID)
    (asserts! (< (len synopsis) u129) RESP_TITLE_INVALID)
    (asserts! (are-topics-valid? topics) RESP_TITLE_INVALID)

    ;; Record asset data
    (map-insert asset-catalog
      { asset-id: next-id }
      {
        title: title,
        publisher: tx-sender,
        storage-size: filesize,
        publish-time: block-height,
        genre: genre,
        synopsis: synopsis,
        topics: topics
      }
    )

    ;; Grant publisher access
    (map-insert permission-ledger
      { asset-id: next-id, viewer: tx-sender }
      { 
        authorized: true,
        authorizer: tx-sender,
        timestamp: block-height
      }
    )

    ;; Update system counter
    (var-set asset-tracker next-id)
    (ok next-id)
  )
)


;;-----------------------------------------------------------------------------
;; User Preference Functions
;;-----------------------------------------------------------------------------

;; Add asset to personal bookmarks
(define-public (bookmark-asset (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? asset-catalog { asset-id: asset-id }) RESP_ASSET_MISSING))
    )
    ;; Validation checks
    (asserts! (asset-exists? asset-id) RESP_ASSET_MISSING)
    (asserts! (has-viewing-rights? asset-id tx-sender) RESP_NO_ACCESS)
    (asserts! (not (is-bookmarked? asset-id tx-sender)) RESP_BOOKMARK_EXISTS)

    ;; Record bookmark
    (map-insert bookmark-registry
      { viewer: tx-sender, asset-id: asset-id }
      {
        created: block-height,
        modified: block-height
      }
    )
    (ok true)
  )
)

;; Remove asset from personal bookmarks
(define-public (remove-bookmark (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? asset-catalog { asset-id: asset-id }) RESP_ASSET_MISSING))
    )
    ;; Validation checks
    (asserts! (asset-exists? asset-id) RESP_ASSET_MISSING)
    (asserts! (is-bookmarked? asset-id tx-sender) RESP_NOT_BOOKMARKED)

    ;; Remove bookmark
    (map-delete bookmark-registry { viewer: tx-sender, asset-id: asset-id })
    (ok true)
  )
)

;; Check bookmark status for current user
(define-read-only (get-bookmark-status (asset-id uint))
  (ok (is-bookmarked? asset-id tx-sender))
)



;;-----------------------------------------------------------------------------
;; Access Control Functions
;;-----------------------------------------------------------------------------

;; Grant asset access permissions
(define-public (authorize-viewer (asset-id uint) (viewer principal))
  (let
    (
      (asset-data (unwrap! (map-get? asset-catalog { asset-id: asset-id }) RESP_ASSET_MISSING))
    )
    ;; Validation checks
    (asserts! (asset-exists? asset-id) RESP_ASSET_MISSING)
    (asserts! (is-publisher? asset-id tx-sender) RESP_PERMISSION_DENIED)
    (asserts! (not (is-eq viewer tx-sender)) RESP_INVALID_SHARING)

    ;; Grant access
    (map-set permission-ledger
      { asset-id: asset-id, viewer: viewer }
      { 
        authorized: true,
        authorizer: tx-sender,
        timestamp: block-height
      }
    )
    (ok true)
  )
)

;; Remove access permissions
(define-public (deauthorize-viewer (asset-id uint) (viewer principal))
  (let
    (
      (asset-data (unwrap! (map-get? asset-catalog { asset-id: asset-id }) RESP_ASSET_MISSING))
      (permission-data (unwrap! (map-get? permission-ledger { asset-id: asset-id, viewer: viewer }) RESP_PERMISSION_DENIED))
    )
    ;; Validation checks
    (asserts! (asset-exists? asset-id) RESP_ASSET_MISSING)
    (asserts! (is-publisher? asset-id tx-sender) RESP_PERMISSION_DENIED)
    (asserts! (not (is-eq viewer tx-sender)) RESP_INVALID_SHARING)

    ;; Remove permissions
    (map-delete permission-ledger { asset-id: asset-id, viewer: viewer })

    ;; Clean up bookmarks if present
    (if (is-bookmarked? asset-id viewer)
      (map-delete bookmark-registry { viewer: viewer, asset-id: asset-id })
      true
    )
    (ok true)
  )
)

;; Verify user access rights
(define-read-only (verify-access (asset-id uint) (viewer principal))
  (ok (has-viewing-rights? asset-id viewer))
)


;;-----------------------------------------------------------------------------
;; Ownership Management Functions
;;-----------------------------------------------------------------------------

;; Transfer asset ownership
(define-public (transfer-asset (asset-id uint) (new-publisher principal))
  (let
    (
      (asset-data (unwrap! (map-get? asset-catalog { asset-id: asset-id }) RESP_ASSET_MISSING))
    )
    ;; Validation checks
    (asserts! (asset-exists? asset-id) RESP_ASSET_MISSING)
    (asserts! (is-publisher? asset-id tx-sender) RESP_PERMISSION_DENIED)
    (asserts! (not (is-eq new-publisher tx-sender)) RESP_INVALID_SHARING)
    (asserts! (is-valid-user? new-publisher) RESP_INVALID_USER)

    ;; Update ownership record
    (map-set asset-catalog
      { asset-id: asset-id }
      (merge asset-data { publisher: new-publisher })
    )

    ;; Update access permissions
    (map-set permission-ledger
      { asset-id: asset-id, viewer: new-publisher }
      {
        authorized: true,
        authorizer: tx-sender,
        timestamp: block-height
      }
    )
    (ok true)
  )
)

;;-----------------------------------------------------------------------------
;; Asset Update Functions
;;-----------------------------------------------------------------------------

;; Modify existing asset details
(define-public (update-asset (asset-id uint) (new-title (string-ascii 64)) (new-size uint) (new-genre (string-ascii 32)) (new-synopsis (string-ascii 128)) (new-topics (list 10 (string-ascii 32))))
  (let
    (
      (asset-data (unwrap! (map-get? asset-catalog { asset-id: asset-id }) RESP_ASSET_MISSING))
    )
    ;; Validate ownership and input
    (asserts! (asset-exists? asset-id) RESP_ASSET_MISSING)
    (asserts! (is-eq (get publisher asset-data) tx-sender) RESP_PERMISSION_DENIED)
    (asserts! (> (len new-title) u0) RESP_TITLE_INVALID)
    (asserts! (< (len new-title) u65) RESP_TITLE_INVALID)
    (asserts! (> new-size u0) RESP_FILESIZE_INVALID)
    (asserts! (< new-size u1000000000) RESP_FILESIZE_INVALID)
    (asserts! (> (len new-genre) u0) RESP_GENRE_INVALID)
    (asserts! (< (len new-genre) u33) RESP_GENRE_INVALID)
    (asserts! (> (len new-synopsis) u0) RESP_TITLE_INVALID)
    (asserts! (< (len new-synopsis) u129) RESP_TITLE_INVALID)
    (asserts! (are-topics-valid? new-topics) RESP_TITLE_INVALID)

    ;; Update asset record
    (map-set asset-catalog
      { asset-id: asset-id }
      (merge asset-data { 
        title: new-title, 
        storage-size: new-size, 
        genre: new-genre, 
        synopsis: new-synopsis, 
        topics: new-topics 
      })
    )
    (ok true)
  )
)

;;-----------------------------------------------------------------------------
;; Asset Removal Functions
;;-----------------------------------------------------------------------------

;; Remove asset from platform
(define-public (remove-asset (asset-id uint))
  (let
    (
      (asset-data (unwrap! (map-get? asset-catalog { asset-id: asset-id }) RESP_ASSET_MISSING))
    )
    ;; Validation checks
    (asserts! (asset-exists? asset-id) RESP_ASSET_MISSING)
    (asserts! (is-eq (get publisher asset-data) tx-sender) RESP_PERMISSION_DENIED)

    ;; Delete asset record
    (map-delete asset-catalog { asset-id: asset-id })
    (ok true)
  )
)


