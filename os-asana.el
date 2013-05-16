; asana-login-auth-basic is defined in .emacs
(eval-when-compile (require 'cl))
(require 'url)
(require 'os)
(require 'json)

(defvar os-asana-backend
  '((base-url      . os-asana-base-url)
    (fetch-buglist . os-asana-fetch-buglist)
    (send-buglist  . os-asana-send-buglist))
  "Asana backend.")

(defvar os-asana-auth nil
  "Base64 encoded API key as username with Basic Authentication. Should look like: 'Basic 9sjOIJIJOIJ987HIUBI879'")

(defvar os-asana-url nil
  "URL of the tasks for your workspace.")

(defun os-asana-base-url (url)
  "Return proper URL."
  "https://app.asana.com/api/1.0/")

(defun os-asana-fetch-buglist (last-update)
  "Fetch buglist from asana.com (anything that happened after LAST-UPDATE)"
  ;; a buglist is just a plist
  (let* ((json (os-asana-fetch-json (concat os-asana-url "?assignee=me"))))

    `(:title "Asana Tasks"
             :url ,os-base-url

             ;; add a :since property set to last-update if you return
             ;; only the bugs updated since it.  omit it or set it to
             ;; nil if you ignore last-update and fetch all the bugs of
             ;; the repo.

             ;; bugs contains a list of bugs
             ;; a bug is a plist too
             :bugs ,(mapcar 'os-asana-json-to-bug (cdr (assoc 'data json))))))


(defun os-asana-fetch-json (url)
  "Return a parsed JSON object of all the pages of URL."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,os-asana-auth)))
         (buf (url-retrieve-synchronously url)))

    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer buf)))))


(defun os-asana-json-to-bug (data)
  "Return DATA (in json) converted to a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key data)))
    (let* ((title (v 'name))
           (id (v 'id)))

      `(:id ,id
            :title ,title))))

;; this overrides os--send-buglist
(defun os-asana-send-buglist (buglist)
  "Send BUGLIST to asana.com and return updated buglist"
  ;; here you should loop over :bugs in buglist

  (dolist (b (os-get-prop :bugs buglist))
    (cond
     ;; new bug (no id    json)))
     ((null (os-get-prop :id b))
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Authorization" . ,os-asana-auth)))
             (url-request-data (os-asana-bug-to-json b))
             (buf (url-retrieve-synchronously os-asana-url))))

      ;; else, modified bug
      ;;     (t
      ;;      '(do-stuff))))
      )))
  ;; return any bug that has changed (modification date, new bugs,
  ;; etc).  they will overwrite/be added in the buglist in os.el
  "Made it")

(defun os-asana-bug-to-json (bug)
  "Return BUG as JSON."

  (json-encode
   `((name . ,(os-get-prop :title bug))
     (notes . ,(os-get-prop :desc bug))
     (assignee . "me"))))
