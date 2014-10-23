; asana-login-auth-basic is defined in .emacs
(eval-when-compile (require 'cl))
(require 'url)
(require 'os)
(require 'json)

(defvar org-sync-asana-backend
  '((base-url      . org-sync-asana-base-url)
    (fetch-buglist . org-sync-asana-fetch-buglist)
    (send-buglist  . org-sync-asana-send-buglist))
  "Asana backend.")

(defvar org-sync-asana-auth nil
  "Base64 encoded API key as username with Basic Authentication. Should look like: 'Basic 9sjOIJIJOIJ987HIUBI879'")

(defvar org-sync-asana-url nil
  "URL of the tasks for your workspace.")

(defvar org-sync-asana-base-url "https://app.asana.com/api/1.0/"
  "Return proper URL.")

(defun org-sync-asana-fetch-buglist (last-update)
  "Fetch buglist from asana.com (anything that happened after LAST-UPDATE)"
  ;; a buglist is just a plist
  (let* ((json (org-sync-asana-fetch-json (concat org-sync-asana-base-url "workspaces/" org-sync-asana-workspace-id "/tasks?assignee=me")))
         (bugs (mapcar
                '(lambda (x)  (org-sync-asana-fetch-json (concat org-sync-asana-base-url "tasks/" (number-to-string x))))
                (mapcar
                 '(lambda (x) (cdr (assoc 'id x)))
                 (cdr (assoc 'data json))))))

    `(:title "Asana Tasks"
             :url ,org-sync-base-url

             ;; add a :since property set to last-update if you return
             ;; only the bugs updated since it.  omit it or set it to
             ;; nil if you ignore last-update and fetch all the bugs of
             ;; the repo.

             ;; bugs contains a list of bugs
             ;; a bug is a plist too
             :bugs ,(mapcar 'org-sync-asana-json-to-bug bugs))))


(defun org-sync-asana-fetch-json (url)
  "Return a parsed JSON object of all the pages of URL."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,org-sync-asana-auth)))
         (buf (url-retrieve-synchronously url)))

    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer buf)))))


(defun org-sync-asana-json-to-bug (data)
  "Return DATA (in json) converted to a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key (cdr (assoc 'data data)))))
    (let* ((title (v 'name))
           (id (v 'id))
           (desc (v 'notes))
           (status (cond
                    ((eq (v 'completed) :json-false) 'open)
                    ((eq (v 'completed) t) 'closed))))

      `(:id ,id
            :title ,title
            :status ,status
            :desc ,desc))))

;; this overrides org-sync--send-buglist
(defun org-sync-asana-send-buglist (buglist)
  "Send BUGLIST to asana.com and return updated buglist"
  ;; here you should loop over :bugs in buglist

  (dolist (b (org-sync-get-prop :bugs buglist))
    (cond
     ;; new bug (no id    json)))
     ((null (org-sync-get-prop :id b))
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Authorization" . ,org-sync-asana-auth)))
             (url-request-data (org-sync-asana-bug-to-json b))
             (buf (url-retrieve-synchronously org-sync-asana-url))))

      ;; else, modified bug
      ;;     (t
      ;;      '(do-stuff))))
      )))
  ;; return any bug that has changed (modification date, new bugs,
  ;; etc).  they will overwrite/be added in the buglist in os.el
  "Made it")

(defun org-sync-asana-bug-to-json (bug)
  "Return BUG as JSON."

  (json-encode
   `((name . ,(org-sync-get-prop :title bug))
     (notes . ,(org-sync-get-prop :desc bug))
     (assignee . "me"))))
