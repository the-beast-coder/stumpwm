(in-package :stumpwm)

;;; Startup Programs
;;; Set Background
(run-shell-command "sh ~/.xprofile")

(load "~/quicklisp/setup.lisp")

(setf *message-window-gravity* :bottom-right
      *input-window-gravity* :bottom-right
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
(if (not swm-gaps:*gaps-on*)
    (swm-gaps:toggle-gaps))
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
(setf *window-format* "%m%n%s%20t ")
(setf *mode-line-timeout* 1)

(setf stumpwm:*screen-mode-line-format*
      (list "^B^3 %g ^n^b %W ^> "
            " "
            '(:eval *modelineinfo*)))

(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* "#000809")
(setf *mode-line-foreground-color* "DeepSkyBlue")

;; turn on/off the mode line for the current head only.
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))

(stumpwm:grename "1")
(stumpwm:gnewbg "2")
(stumpwm:gnewbg "3")
(stumpwm:gnewbg "4")
(stumpwm:gnewbg "5")
(stumpwm:gnewbg "6")
(stumpwm:gnewbg "7")
(stumpwm:gnewbg "8")
(stumpwm:gnewbg "9")

;; I change the prefix key to something else besides a keychord.
;; The following three lines are a dirty hack to make SUPER the prefix key.
;; This was originally (set-prefix-key (kbd "C-t"))
(run-shell-command "xmodmap -e 'clear mod4'" t)
(run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
(set-prefix-key (kbd "F20"))

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; workspace keybinds

(defvar *move-to-keybinds* (list "!" "@"  "#" "$" "%" "^" "&" "*" "("))
(dotimes (y 9)
  (let ((workspace (write-to-string (+ y 1))))
    (define-key *root-map* (kbd workspace) (concat "gselect " workspace))
    (define-key *root-map* (kbd (nth y *move-to-keybinds*)) (concat "gmove-and-follow " workspace))))

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

(define-key *root-map* (kbd "Q") "better-quit")
(define-key *root-map* (kbd "C-r") "better-restart")

(defcommand increase-gaps () ()
  (setf swm-gaps:*outer-gaps-size* (+ swm-gaps:*outer-gaps-size* 5)
        swm-gaps:*inner-gaps-size* (+ swm-gaps:*inner-gaps-size* 5))
  (swm-gaps:toggle-gaps)
  (swm-gaps:toggle-gaps))

(defcommand decrease-gaps () ()
  (if (> swm-gaps:*inner-gaps-size* 5)
      (progn
        (setf swm-gaps:*outer-gaps-size* (- swm-gaps:*outer-gaps-size* 5)
              swm-gaps:*inner-gaps-size* (- swm-gaps:*inner-gaps-size* 5))
        (swm-gaps:toggle-gaps)
        (swm-gaps:toggle-gaps))))

(define-key *root-map* (kbd "g") "toggle-gaps")
(define-key *root-map* (kbd "X") "increase-gaps")
(define-key *root-map* (kbd "Z") "decrease-gaps")

;; (define-key *root-map* (kbd "h") "move-focus left")
;; (define-key *root-map* (kbd "j") "move-focus down")
;; (define-key *root-map* (kbd "k") "move-focus up")
;; (define-key *root-map* (kbd "l") "move-focus left")

(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")

(define-key *root-map* (kbd "'") "windowlist")

(define-key *root-map* (kbd "C-h") '*help-map*)

(define-key *root-map* (kbd "q") "delete")
(define-key *root-map* (kbd "r") "remove")
(define-key *root-map* (kbd "R") "iresize")

(define-key *root-map* (kbd "z") "delete")

(setf *resize-increment* 25)
(define-key *top-map* (kbd "M-l") "resize-direction Right")
(define-key *top-map* (kbd "M-h") "resize-direction Left")
(define-key *top-map* (kbd "M-k") "resize-direction Up")
(define-key *top-map* (kbd "M-j") "resize-direction Down")

(define-key *root-map* (kbd "C-m") "mode-line")

(define-key *root-map* (kbd "RET") "exec st") 

;; Launch Dmenu
;; (define-key *root-map* (kbd "d") "exec dmenu_run")
;; (define-key *top-map* (kbd "M-space") "exec dmenu_run -c -i -g 4 -l 7")

(define-key *root-map* (kbd "space") "exec")
(define-key *root-map* (kbd "M-space") "exec")

(defvar *aadi/layouts-map* (make-sparse-keymap)
  "Layouts to set for windows")
(define-key *root-map* (kbd "[") '*aadi/layouts-map*)
(define-key *aadi/layouts-map* (kbd "g") "restore-from-file ~/.config/stumpwm/layouts/grid")
(define-key *aadi/layouts-map* (kbd "3") "restore-from-file ~/.config/stumpwm/layouts/3layout")
(define-key *aadi/layouts-map* (kbd "4") "restore-from-file ~/.config/stumpwm/layouts/4layout")
(define-key *aadi/layouts-map* (kbd "t") "float-this")
(define-key *aadi/layouts-map* (kbd "T") "unfloat-this")

;; (setf *ignore-wm-inc-hints* t)
;; (defun show-key-seq (key seq val)
;;   (message (print-key-seq (reverse seq))))

;; (stumpwm:add-hook stumpwm:*key-press-hook* 'show-key-seq)

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

(defvar *aadi/emacs-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")

(define-key *root-map* (kbd "e") '*aadi/emacs-map*)
(define-key *aadi/emacs-map* (kbd "e") "exec emacsclient -c -a ''")
(define-key *aadi/emacs-map* (kbd "f") "exec emacsclient -c -a '' ~")
(define-key *aadi/emacs-map* (kbd "c") "exec emacsclient -c -a '' ~/.config/")
(define-key *aadi/emacs-map* (kbd "w") "exec emacsclient -c -a '' ~/Documents/emacs-wiki/main.org")
(define-key *aadi/emacs-map* (kbd "s") "exec emacsclient -c -a '' ~/Documents/some-code")
(define-key *aadi/emacs-map* (kbd "m") "exec emacsclient -c -a '' ~/.config/stumpwm/config")

(ql:quickload "cl-ppcre")
(defcommand aadi/yt-search () ()
  (run-shell-command
   (concat "brave --incognito --new-window youtube.com/results?search_query="
           (cl-ppcre:regex-replace-all " "
                                       (completing-read (current-screen)
                                                        "Youtube search: "
                                                        (list "Asmongold" "Gothamchess" "Distrotube"))
                                       "+"))))

(defvar *aadi/browser-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")
(define-key *root-map* (kbd "w") '*aadi/browser-map*)
(define-key *aadi/browser-map* (kbd "w") "exec brave")
(define-key *aadi/browser-map* (kbd "y") "aadi/yt-search")
(define-key *aadi/browser-map* (kbd "i") "exec brave --incognito --new-window")
(define-key *aadi/browser-map* (kbd "p") "exec brave --incognito --new-window")

(defvar *aadi/scripts-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")
(define-key *root-map* (kbd "a") '*aadi/scripts-map*)
(define-key *aadi/scripts-map* (kbd "h") "exec st -e htop")
(define-key *aadi/scripts-map* (kbd "f") "exec st -e ranger")
(define-key *aadi/scripts-map* (kbd "r") "exec ramusage")

(define-key *top-map* (kbd "XF86AudioMute") "exec pamixer -t")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pamixer --allow-boost -i 5")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pamixer --allow-boost -d 5")

(define-key *root-map* (kbd "C-Right") "exec brightnessctl set 7%+")
(define-key *root-map* (kbd "C-Left") "exec brightnessctl set 7%-")

;;; Define window placement policy...
;; Clear rules
;;(clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;;
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
(define-frame-preference "Default"
    ;; frame raise lock (lock AND raise == jumpto)
    (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
  (1 t nil :class "XTerm"))

(define-frame-preference "Ardour"
    (0 t   t   :instance "ardour_editor" :type :normal)
  (0 t   t   :title "Ardour - Session Control")
  (0 nil nil :class "XTerm")
  (1 t   nil :type :normal)
  (1 t   t   :instance "ardour_mixer")
  (2 t   t   :instance "jvmetro")
  (1 t   t   :instance "qjackctl")
  (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "Shareland"
    (0 t   nil :class "XTerm")
  (1 nil t   :class "aMule"))