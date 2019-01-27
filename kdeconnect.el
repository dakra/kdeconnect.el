;;; kdeconnect.el --- Utility functions for working with kdeconnect -*- lexical-binding: t -*-

;; Copyright (c) 2019 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/kdeconnect.el
;; Keywords: kdeconnect, mobile, phone, convenience, tools
;; Version: 0.1
;; Package-Requires: ((emacs "25.2"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `kdeconnect.el' provides utilities for working with KDE Connect.
;; From the KDE Connect website about what it is:
;; https://community.kde.org/KDEConnect
;; KDE Connect is a project that enables all your devices to communicate with each other.
;; Here's a few things KDE Connect can do:

;; - Receive your phone notifications on your desktop computer and reply to messages
;; - Control music playing on your desktop from your phone
;; - Use your phone as a remote control for your desktop
;; - Run predefined commands on your PC from connected devices.
;; - Check your phones battery level from the desktop
;; - Ring your phone to help finding it
;; - Share files and links between devices
;; - Browse your phone from the desktop
;; - Control the desktop's volume from the phone

;;; Code:

(require 'dbus)


;;; Customization

(defgroup kdeconnect nil
  "KDE Connect integration"
  :prefix "kdeconnect-"
  :group 'tools)

(defcustom kdeconnect-device-id nil
  "KDE Connect ID of the device you want the commands to work on."
  :type 'string)

(defcustom kdeconnect-mode-line-function #'kdeconnect-mode-line-function
  "Function that takes the battery status and charge as argument and set `kdeconnect-mode-line-string'."
  :type 'function)


;;; KDE Connect mode line

(defvar kdeconnect-mode-line-string ""
  "String to show in the modeline.
It should not be set directly, but is instead updated by the
`kdeconnect-mode-line-function' function.")

(defun kdeconnect-mode-line-function (charging-p charge)
  "Set `kdeconnect-mode-line-string' according to CHARGING-P and CHARGE."
  (let ((name (kdeconnect-get-device-name))
        (charging (if charging-p "charging" "discharging")))
    (setq kdeconnect-mode-line-string
          (propertize
           (format "[%s%%]" charge)
           'help-echo (format "%s: %s%% (%s)" name charge charging)))))


;;; Private helper functions

(defun kdeconnect--get-device-ids ()
  "Return list of device IDs."
  (dbus-introspect-get-node-names :session
    "org.kde.kdeconnect"
    "/modules/kdeconnect/devices"))

(defun kdeconnect--get-device-info (id)
  "Return alist with info about device ID."
  (dbus-get-all-properties :session
    "org.kde.kdeconnect"
    (concat "/modules/kdeconnect/devices/" id)
    "org.kde.kdeconnect.device"))

(defun kdeconnect--battery-state-change (charging-p)
  "Function to run when the battery charging state CHARGING-P changes."
  (funcall kdeconnect-mode-line-function charging-p (kdeconnect--call-plugin "battery" "charge")))

(defun kdeconnect--battery-charge-change (charge)
  "Function to run when the battery CHARGE changes."
  (funcall kdeconnect-mode-line-function (kdeconnect--call-plugin "battery" "isCharging") charge))

(defun kdeconnect-get-device-name ()
  "Return the name of the current device."
  (cdr (assoc "name" (kdeconnect--get-device-info (kdeconnect-get-device-id)))))

(defun kdeconnect--call-method (type method &rest args)
  "Make a DBUS call to kdeconnect with type TYPE to method METHOD and optional ARGS."
  (let ((path (concat "/modules/kdeconnect/devices/" (kdeconnect-get-device-id) "/" type))
        (interface (concat "org.kde.kdeconnect.device." type)))
    (apply #'dbus-call-method :session
           "org.kde.kdeconnect" path interface method args)))

(defun kdeconnect--call-plugin (plugin method &rest args)
  "Make a DBUS call to kdeconnect with PLUGIN to method METHOD and optional ARGS."
  (let ((path (concat "/modules/kdeconnect/devices/" (kdeconnect-get-device-id)))
        (interface (concat "org.kde.kdeconnect.device." plugin)))
    (apply #'dbus-call-method :session
           "org.kde.kdeconnect" path interface method args)))

(defun kdeconnect-get-device-id ()
  "Return `kdeconnect-device-id' if set, or let the user choose from a list."
  (if kdeconnect-device-id
      kdeconnect-device-id
    (let ((device-ids (kdeconnect--get-device-ids)))
      (unless device-ids
        (user-error "No device found"))
      (if (= (length device-ids) 1)
          (setq kdeconnect-device-id (car device-ids))
        (setq kdeconnect-device-id (completing-read "Device ID: " device-ids))))))


;;; Interactive functions

;;;###autoload
(defun kdeconnect-ping ()
  "Send a ping to your device."
  (interactive)
  (kdeconnect--call-method "ping" "sendPing"))

;;;###autoload
(defun kdeconnect-message (msg)
  "Send a message MSG to your device."
  (interactive "sMessage: ")
  (kdeconnect--call-method "ping" "sendPing" msg))

;;;###autoload
(defun kdeconnect-find-my-phone ()
  "Rings your device."
  (interactive)
  (kdeconnect--call-method "findmyphone" "ring"))

;;;###autoload
(defun kdeconnect-share-url (url)
  "Send URL to your device.
Prompt user for URL or use URL under point if there is one."
  (interactive (list (or (thing-at-point 'url) (read-string "URL: "))))
  (kdeconnect--call-method "share" "shareUrl" url))

;;;###autoload
(defun kdeconnect-send-sms (number text)
  "Send SMS to NUMBER with TEXT.
Prompt user for TEXT or use region if any is selected."
  (interactive (list (read-string "Phone Number: ")
                     (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-string "Message: "))))
  (kdeconnect--call-method "telephony" "sendSms" number text))

;;;###autoload
(defun kdeconnect-send-key-press (text)
  "Send TEXT as a sequence of key presses to your device.
Prompt user for TEXT or use region if any is selected.
Note that you have to activate the kdeconnect keyboard on your device first."
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (read-string "Text: "))))
  (kdeconnect--call-method "remotekeyboard" "sendKeyPress" text))


;;; Minor mode

;;;###autoload
(define-minor-mode kdeconnect-mode
  "Display the battery status of your device in the mode line."
  :global t
  :group 'kdeconnect
  (unless global-mode-string (setq global-mode-string '("")))
  (if kdeconnect-mode
      (progn
        (dbus-register-signal :session
          "org.kde.kdeconnect"
          "/modules/kdeconnect/devices/41edfb9afd1807ba"
          "org.kde.kdeconnect.device.battery"
          "stateChanged"
          #'kdeconnect--battery-state-change)

        (dbus-register-signal :session
          "org.kde.kdeconnect"
          "/modules/kdeconnect/devices/41edfb9afd1807ba"
          "org.kde.kdeconnect.device.battery"
          "chargeChanged"
          #'kdeconnect--battery-charge-change)
        (funcall kdeconnect-mode-line-function (kdeconnect--call-plugin "battery" "isCharging") (kdeconnect--call-plugin "battery" "charge"))
        (add-to-list 'global-mode-string 'kdeconnect-mode-line-string t))

    (dbus-unregister-service :session
      "org.kde.kdeconnect")
    (setq global-mode-string
	  (delq 'kdeconnect-mode-line-string global-mode-string))))

(provide 'kdeconnect)
;;; kdeconnect.el ends here
