(in-package :stumpwm)
(load "~/quicklisp/setup.lisp")

(run-shell-command "xmodmap -e 'clear mod4'" t)
(run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
(run-shell-command "sh ~/.xprofile")
;; Change the prefix to the super key
(set-prefix-key (kbd "F20"))

(load-module "ttf-fonts")
(ql:quickload :clx-truetype)
(clx-truetype:cache-fonts)

(set-font (make-instance 'xft:font
                         :family "JetBrains Mono"
                         :subfamily "Regular"
                         :size 9
                         :antialias t))

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

(load-module "swm-gaps")
(if (not swm-gaps:*gaps-on*)
    (swm-gaps:toggle-gaps))
(setf swm-gaps:*inner-gaps-size* 8
      swm-gaps:*outer-gaps-size* 15
      swm-gaps:*head-gaps-size* 8)

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

(run-shell-command "xmodmap -e 'clear mod4'" t)
(run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
(run-shell-command "st &" t)

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

;; workspace keybinds
(defvar *move-to-keybinds* (list "!"
                                 "@"
                                 "#"
                                 "$"
                                 "%"
                                 "^"
                                 "&"
                                 "*"
                                 "("))
(dotimes (y 9)
  (let ((workspace (write-to-string (+ y 1))))
    (define-key *root-map* (kbd workspace) (concat "gselect " workspace))
    (define-key *root-map* (kbd (nth y *move-to-keybinds*)) (concat "gmove-and-follow " workspace))))

(defcommand better-quit () ()
  (kill-all-threads)
  (eval-command "quit"))

(defcommand better-restart () ()
  (kill-all-threads)
  (eval-command "restart-hard"))

(define-key *root-map* (kbd "Q") "better-quit")
(define-key *root-map* (kbd "C-r") "better-restart")

(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")

(define-key *root-map* (kbd "C-h") '*help-map*)

(define-key *root-map* (kbd "q") "delete")
(define-key *root-map* (kbd "r") "remove")
(define-key *root-map* (kbd "R") "iresize")

(setf *resize-increment* 25)
(define-key *top-map* (kbd "M-l") "resize-direction Right")
(define-key *top-map* (kbd "M-h") "resize-direction Left")
(define-key *top-map* (kbd "M-k") "resize-direction Up")
(define-key *top-map* (kbd "M-j") "resize-direction Down")

(define-key *root-map* (kbd "d") "exec dmenu_run")
(define-key *top-map* (kbd "M-space") "exec dmenu_run -c -i -g 4 -l 7")

(defvar *aadi/emacs-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")
(define-key *root-map* (kbd "e") '*aadi/emacs-map*)
(define-key *aadi/emacs-map* (kbd "e") "exec emacsclient -c -a ''")
(define-key *aadi/emacs-map* (kbd "f") "exec emacsclient -c -a '' ~")
(define-key *aadi/emacs-map* (kbd "c") "exec emacsclient -c -a '' ~/.config/")
(define-key *aadi/emacs-map* (kbd "w") "exec emacsclient -c -a '' ~/Documents/emacs-wiki/main.org")
(define-key *aadi/emacs-map* (kbd "s") "exec emacsclient -c -a '' ~/Documents/some-code")
(define-key *aadi/emacs-map* (kbd "m") "exec emacsclient -c -a '' ~/.config/stumpwm/config")

(defvar *aadi/browser-map* (make-sparse-keymap)
  "Keymap for finding files (and doing other things) in emacs.")
(define-key *root-map* (kbd "w") '*aadi/browser-map*)
(define-key *aadi/browser-map* (kbd "w") "exec brave")
(define-key *aadi/browser-map* (kbd "y") "exec yt")
(define-key *aadi/browser-map* (kbd "i") "exec brave --incognito --new-window")
(define-key *aadi/browser-map* (kbd "p") "exec brave --incognito --new-window")

(define-key *root-map* (kbd "RET") "exec st")
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

(defvar *aadi/layouts-map* (make-sparse-keymap)
  "Layouts to set for windows")
(define-key *root-map* (kbd "[") '*aadi/layouts-map*)
(define-key *aadi/layouts-map* (kbd "g") "restore-from-file ~/.config/stumpwm/layouts/grid")
(define-key *aadi/layouts-map* (kbd "3") "restore-from-file ~/.config/stumpwm/layouts/3layout")
(define-key *aadi/layouts-map* (kbd "4") "restore-from-file ~/.config/stumpwm/layouts/4layout")
(define-key *aadi/layouts-map* (kbd "t") "float-this")
(define-key *aadi/layouts-map* (kbd "T") "unfloat-this")

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
