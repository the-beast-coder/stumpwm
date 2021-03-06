(in-package :stumpwm)

;;; Startup Programs
;;; Set Background
(run-shell-command "sh ~/.xprofile")

(load "~/quicklisp/setup.lisp")

;;; Changing the prefix key to the super key
;; Clear the super key
(run-shell-command "xmodmap -e 'clear mod4'" t)
;; Bind the super key to F20
(run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
;; Set the prefix key to F20
(set-prefix-key (kbd "F20"))

(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :thin
      *message-window-padding* 10
      *maxsize-border-width* 2
      *normal-border-width* 2
      *transient-border-width* 2
      stumpwm::*float-window-border* 4
      stumpwm::*float-window-title-height* 20
      *mouse-focus-policy* :click)

(load-module "ttf-fonts")
(ql:quickload :clx-truetype)
(clx-truetype:cache-fonts)

(set-font (make-instance 'xft:font
                         :family "JetBrains Mono"
                         :subfamily "Regular"
                         :size 10
                         :antialias t))

(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 7
      swm-gaps:*outer-gaps-size* 12
      swm-gaps:*head-gaps-size* 0)

(defvar *modelineinfo* "battery | date")
(defparameter *threads* '())  ; we stock all threads here

;; Helper to create and stock thread
(defun do-in-thread (fn)
  (setf *threads*
        (cons
         (sb-thread:make-thread fn)
         *threads*)))

;; Create some threads that loop forever
(do-in-thread
    (lambda ()
      (loop do
        (progn
          (setf *modelineinfo* (run-shell-command "sh ~/.config/stumpwm/modeline.sh" t))
          (sleep 20)))))

;; Call this function when quiting stumpwm
(defun kill-all-threads ()
  (loop for th in *threads*
        do
           (if (sb-thread:thread-alive-p th) (sb-thread:terminate-thread th))))

(setf *bar-med-color* "^B^8")
(setf *bar-hi-color* "^B^3")
(setf *bar-crit-color* "^B^1")

(setf *colors*
      '("black"
        "red"
        "green"
        "yellow"
        "blue"
        "magenta"
        "cyan"
        "white"
        "GreenYellow"
        "#009696"))

(update-color-map (current-screen))

(setf *group-format* " %t ")
;; (setf *window-format* "%m%50t ")
(setf *window-format* "%m%n%s%18t ")
(setf *mode-line-timeout* 1)

(setf stumpwm:*screen-mode-line-format*
      (list "^B^3 %g ^n^b %W ^> "
            " "
            '(:eval *modelineinfo*)))

(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* "#000809")
(setf *mode-line-foreground-color* "DeepSkyBlue")

;; Turn on the modeline for all heads
(dolist (head
         (list (first (screen-heads (current-screen)))))
  (enable-mode-line (current-screen) head
                    t *screen-mode-line-format*))

(defvar *aadi/workspaces* (list "WWW" "Emacs" "Term"))
(stumpwm:grename (nth 0 *aadi/workspaces*))
(dolist (workspace (cdr *aadi/workspaces*))
  (stumpwm:gnewbg workspace))

(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

(defcommand better-restart () ()
  (kill-all-threads)
  (eval-command "restart-hard"))

(defcommand better-quit () ()
  (let ((output (string-downcase (completing-read
                                  (current-screen)
                                  "Command: "
                                  (list "restart" "shutdown" "log out" "suspend" "sleep" "hibernate")))))
    (if (string/= output "")
        (cond ((string= output "restart")
               (kill-all-threads)
               (run-shell-command "reboot"))
              ((string= output "shutdown")
               (kill-all-threads)           
               (run-shell-command "shutdown -h now"))
              ((string= output "log out")
               (kill-all-threads)
               (eval-command "quit"))
              ((or (string= output "suspend") (string= output "sleep"))
               (run-shell-command "systemctl suspend"))
              ((string= output "hibernate")
               (run-shell-command "systemctl hibernate"))
              (t (echo "Please enter restart, shutdown, log out, suspend or hibernate."))))))

(defcommand increase-gaps () ()
  (setf swm-gaps:*outer-gaps-size* (+ swm-gaps:*outer-gaps-size* 5)
        swm-gaps:*inner-gaps-size* (+ swm-gaps:*inner-gaps-size* 5))
  (swm-gaps:toggle-gaps)
  (swm-gaps:toggle-gaps))

(defcommand decrease-gaps () ()
  (if ((and (> swm-gaps:*inner-gaps-size* 7) (> swm-gaps:*outer-gaps-size* 7)))
      (progn
        (setf swm-gaps:*outer-gaps-size* (- swm-gaps:*outer-gaps-size* 5)
              swm-gaps:*inner-gaps-size* (- swm-gaps:*inner-gaps-size* 5))
        (swm-gaps:toggle-gaps)
        (swm-gaps:toggle-gaps))))

; display the key sequence in progress
;; (defun key-press-hook (key key-seq cmd)
;;   (declare (ignore key))
;;   (unless (eq *top-map* *resize-map*)
;;     (let ((*message-window-gravity* :bottom-right))
;;       (message "Keys: ~a" (print-key-seq (reverse key-seq))))
;;     (when (stringp cmd)
;;       ;; give 'em time to read it
;;       (sleep 0.3))))

;; (defmacro replace-hook (hook fn)
;;   `(remove-hook ,hook ,fn)
;;   `(add-hook ,hook ,fn))

;; (replace-hook *key-press-hook* 'key-press-hook)

(ql:quickload "cl-ppcre")
(defcommand aadi/yt-search () ()
  (run-shell-command
   (let ((search (completing-read (current-screen) "Youtube search: " (list "Asmongold" "Gothamchess" "Distrotube"))))
     (if (string/= search nil)
         (concat "brave --incognito --new-window youtube.com/results?search_query="
                 (cl-ppcre:regex-replace-all " " search "+"))))))

(defcommand (kill-from-windowlist tile-group)
    (&optional (fmt *window-format*)) (:rest)
  (let ((window-to-kill (select-window-from-menu
                        (group-windows (current-group))
                        fmt)))
    (when window-to-kill
      (kill-window window-to-kill))))

(defvar *move-to-keybinds* (list "!" "@"  "#" "$" "%" "^" "&" "*" "("))
(dotimes (y (length *aadi/workspaces*))
  (let ((workspace (write-to-string (+ y 1))))
    (define-key *root-map* (kbd workspace) (concat "gselect " workspace))
    (define-key *root-map* (kbd (nth y *move-to-keybinds*)) (concat "gmove-and-follow " workspace))))

(define-key *root-map* (kbd "Q") "better-quit")
(define-key *root-map* (kbd "C-r") "better-restart")

(define-key *root-map* (kbd "g") "toggle-gaps")
(define-key *root-map* (kbd "X") "increase-gaps")
(define-key *root-map* (kbd "Z") "decrease-gaps")

(define-key *root-map* (kbd "C-h") '*aadi/*help-map*)
(define-key *root-map* (kbd "C-m") "mode-line")

(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")

(define-key *root-map* (kbd "'") "windowlist")

(setf *resize-increment* 25)
(define-key *top-map* (kbd "M-l") "resize-direction Right")
(define-key *top-map* (kbd "M-h") "resize-direction Left")
(define-key *top-map* (kbd "M-k") "resize-direction Up")
(define-key *top-map* (kbd "M-j") "resize-direction Down")

(define-key *root-map* (kbd "q") "delete")
(define-key *root-map* (kbd "r") "remove")
(define-key *root-map* (kbd "R") "iresize")

(define-key *root-map* (kbd "z") "delete")

(defvar *aadi/windows-map* (make-sparse-keymap)
  "Keymap for manipulating windows")

(define-key *root-map* (kbd "b") '*aadi/windows-map*)
(define-key *aadi/windows-map* (kbd "K") "kill-windows-current-group")
(define-key *aadi/windows-map* (kbd "k") "kill-from-windowlist")
(define-key *aadi/windows-map* (kbd "O") "kill-windows-other")
(define-key *aadi/windows-map* (kbd "p") "pull-from-windowlist")
(define-key *aadi/windows-map* (kbd "t") "toggle-always-on-top")
(define-key *aadi/windows-map* (kbd "T") "toggle-always-show")

(define-key *root-map* (kbd "C-k") "kill-from-windowlist")

(define-key *root-map* (kbd "space") "exec")
(define-key *top-map* (kbd "M-space") "exec")
(define-key *top-map* (kbd "M-;") "colon")

(define-key *root-map* (kbd "RET") "exec st")
(defvar *aadi/scripts-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")
(define-key *root-map* (kbd "a") '*aadi/scripts-map*)
(define-key *aadi/scripts-map* (kbd "h") "exec st -e htop")
(define-key *aadi/scripts-map* (kbd "f") "exec st -e ranger")
(define-key *aadi/scripts-map* (kbd "n") "exec st -e nmtui")
(define-key *aadi/scripts-map* (kbd "r") "exec ramusage")
(define-key *aadi/scripts-map* (kbd "y") "exec mpv-yt")
(define-key *aadi/scripts-map* (kbd "N") "exec st -e nmtui")

(defvar *aadi/emacs-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")

(defvar *aadi/editor* "e")

(define-key *root-map* (kbd "e") '*aadi/emacs-map*)
(define-key *aadi/emacs-map* (kbd "e") (concat "exec " *aadi/editor*))
(define-key *aadi/emacs-map* (kbd "f") (concat "exec " *aadi/editor* " ~"))
(define-key *aadi/emacs-map* (kbd "c") (concat "exec " *aadi/editor* " ~/.config/"))
(define-key *aadi/emacs-map* (kbd "w") (concat "exec " *aadi/editor* " ~/Documents/emacs-wiki/main.org"))
(define-key *aadi/emacs-map* (kbd "s") (concat "exec " *aadi/editor* " ~/Documents/some-code"))
(define-key *aadi/emacs-map* (kbd "m") (concat "exec " *aadi/editor* " ~/.config/stumpwm/config.org"))
(define-key *aadi/emacs-map* (kbd "n") "open-notes")

(defvar *aadi/layouts-map-multimonitor* (make-sparse-keymap)
  "Layouts to set for windows")
(define-key *root-map* (kbd "[") '*aadi/layouts-map-monitor*)
(define-key *aadi/layouts-map-multimonitor* (kbd "g") "restore-from-file ~/.config/stumpwm/layouts/multimonitor/grid")
(define-key *aadi/layouts-map-multimonitor* (kbd "3") "restore-from-file ~/.config/stumpwm/layouts/multimonitor/3layout")
(define-key *aadi/layouts-map-multimonitor* (kbd "4") "restore-from-file ~/.config/stumpwm/layouts/multimonitor/4layout")
(define-key *aadi/layouts-map-multimonitor* (kbd "w") "restore-from-file ~/.config/stumpwm/layouts/multimonitor/web")
(define-key *aadi/layouts-map-multimonitor* (kbd "t") "float-this")
(define-key *aadi/layouts-map-multimonitor* (kbd "T") "unfloat-this")

(defvar *aadi/layouts-map-singlemonitor* (make-sparse-keymap)
  "Layouts to set for windows")
(define-key *root-map* (kbd "]") '*aadi/layouts-map-monitor*)
(define-key *aadi/layouts-map-singlemonitor* (kbd "g") "restore-from-file ~/.config/stumpwm/layouts/singlemonitor/grid")
(define-key *aadi/layouts-map-singlemonitor* (kbd "3") "restore-from-file ~/.config/stumpwm/layouts/singlemonitor/3layout")
(define-key *aadi/layouts-map-singlemonitor* (kbd "4") "restore-from-file ~/.config/stumpwm/layouts/singlemonitor/4layout")
(define-key *aadi/layouts-map-singlemonitor* (kbd "w") "restore-from-file ~/.config/stumpwm/layouts/singlemonitor/web")
(define-key *aadi/layouts-map-singlemonitor* (kbd "t") "float-this")
(define-key *aadi/layouts-map-singlemonitor* (kbd "T") "unfloat-this")

(defvar *aadi/browser-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")
(define-key *root-map* (kbd "w") '*aadi/browser-map*)
(define-key *aadi/browser-map* (kbd "w") "exec brave")
(define-key *aadi/browser-map* (kbd "y") "aadi/yt-search")
(define-key *aadi/browser-map* (kbd "i") "exec brave --incognito --new-window")
(define-key *aadi/browser-map* (kbd "p") "exec brave --incognito --new-window")

(define-key *top-map* (kbd "XF86AudioMute") "exec pamixer -t")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pamixer --allow-boost -i 5")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pamixer --allow-boost -d 5")

(define-key *root-map* (kbd "C-Right") "exec brightnessctl set 7%+")
(define-key *root-map* (kbd "C-Left") "exec brightnessctl set 7%-")
