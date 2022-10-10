;;; bluesound.el --- Play, pause, resume music on a Bluesound player  -*- lexical-binding: t -*-

;; Copyright (C) 2022 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Version: 0.1
;; Keywords: convenience multimedia
;; URL: https://git.sr.ht/~rwv/bluesound-el/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The packages allows you to control your Bluesound player from
;; Emacs.  To activate it customize `bluesound-host' to point to the
;; IP address or hostname of player in your network.

;;; Code:

(defgroup bluesound nil "Functions to call out to Bluesound player."
  :prefix "bluesound-"
  :group 'multimedia)

(defcustom bluesound-host nil
  "IP address or hostname of Bluesound player API endpoint."
  :group 'bluesound
  :type 'string)

(defcustom bluesound-port 11000
  "Port number of Bluesound player API endpoint."
  :group 'bluesound
  :type 'integer)

(defun bluesound/-GET (path)
  "Do a GET with PATH to player and return parsed XMl."
  (when (not bluesound-host)
    (error "Value of `bluesound-host' unset"))
  (let* ((url (format "http://%s:%d/%s" bluesound-host bluesound-port path)))
    (with-current-buffer (url-retrieve-synchronously url 'silent 'no-cookies 10)
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (forward-line)
      (xml-parse-region (point) (point-max)))))

(defun bluesound/-query (path nodelist)
  "Query PATH on NODELIST and return first matching element."
  (let (result)
    (while (and (not result) nodelist)
      (when (and (listp (car nodelist))
                 (eq (caar nodelist) (car path)))
        (setq result (car nodelist)))
      (setq nodelist (cdr nodelist)))
    (if (cdr path)
        (bluesound/-query (cdr path) result)
      result)))

(defun bluesound/-query-all (path nodelist)
  "Query PATH on NODELIST and return matching elements."
  (let (result)
    (while nodelist
      (when (and (listp (car nodelist))
                 (eq (caar nodelist) (car path)))
        (setq result (cons (car nodelist) result)))
      (setq nodelist (cdr nodelist)))
    (if (cdr path)
        (bluesound/-query-all (cdr path) (cddar result))
      (reverse result))))

(defun bluesound/-attr (name element)
  "Get attribute NAME from ELEMENT."
  (when-let (value (cdr (assoc name (cadr element))))
    (decode-coding-string value 'utf-8)))

(defun bluesound/-text (element)
  "Get text directly inside ELEMENT."
  (let ((node (caddr element)))
    (when (stringp node)
      (decode-coding-string node 'utf-8))))

(defun bluesound/status ()
  "Return status of player as alist."
  (mapcar (lambda (n)
            (cons (car n) (bluesound/-text n)))
          (seq-filter #'listp
                      (cddar (bluesound/-GET "Status")))))

;;;###autoload
(defun bluesound/volume-set (level)
  "Set player volume to LEVEL."
  (interactive "nVolume level: ")
  (bluesound/-GET (concat "Volume?level=" (number-to-string level)))
  (message "Player volume set to: %d" level))

(defun bluesound/volume ()
  "Read volume from player."
  (string-to-number
   (cdr (assoc 'volume (bluesound/status)))))

;;;###autoload
(defun bluesound/volume-up ()
  "Turn player volume level up."
  (interactive)
  (bluesound/volume-set (+ (bluesound/volume) 1)))

;;;###autoload
(defun bluesound/volume-down ()
  "Turn player volume level down."
  (interactive)
  (bluesound/volume-set (- (bluesound/volume) 1)))

;;;###autoload
(defun bluesound/pause ()
  "Pause player."
  (interactive)
  (bluesound/-GET "Pause")
  (message "Player paused"))

;;;###autoload
(defun bluesound/resume ()
  "Resume player."
  (interactive)
  (bluesound/-GET "Play")
  (message "Player resumed"))

;;;###autoload
(defun bluesound/pause-resume ()
  "Toggle between playing and pausing player."
  (interactive)
  (let ((state (cdr (assoc 'state (bluesound/status)))))
    (if (or (equal state "play")
            (equal state "stream"))
        (bluesound/pause)
      (bluesound/resume))))

;;;###autoload
(defun bluesound/play (url)
  "Play an stream from an URL."
  (interactive "sPlay URL: ")
  (bluesound/-GET (concat "Play?url="
                          (url-hexify-string url))))

(defun bluesound/albums-section (section)
  "SECTION is A..Z or #.  Return list of artist/albums pairs."
  (let ((items (bluesound/-query-all
                '(albums sections section album)
                (bluesound/-GET (concat "Albums?service=LocalMusic"
                                        "&section="
                                        (url-hexify-string section))))))
    (when items
      (mapcar (lambda (n)
                (cons (bluesound/-text (bluesound/-query '(art) (cddr n)))
                      (bluesound/-text (bluesound/-query '(title) (cddr n)))))
              items))))

(defvar bluesound/album-cache nil)

(defun bluesound/albums ()
  "Return list of all albums."
  (if bluesound/album-cache
      bluesound/album-cache
    (setq bluesound/album-cache
          (seq-sort-by
           (lambda (n) (downcase (string-trim (concat (car n) (cdr n)))))
           #'string-lessp
           (mapcan (lambda (byte)
                     (bluesound/albums-section
                      (byte-to-string byte)))
                   "#ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))

(defun bluesound/add (album)
  "Add ALBUM to end of player's playlist and start playing."
  (let ((artist-album (split-string album "  /  ")))
    (bluesound/-GET
     (concat "Add?service=LocalMusic&playnow=1&where=last&cursor=last"
             "&artist=" (url-hexify-string (car artist-album))
             "&album=" (url-hexify-string (cadr artist-album))))
    (message "Playing: %s" album)))

;;;###autoload
(defun bluesound/play-album (album)
  "Play an ALBUM on player."
  (interactive
   (list
    (completing-read "Play: "
                     (mapcar (lambda (album)
                               (concat (car album) "  /  " (cdr album)))
                             (bluesound/albums)))))
  (when album
    (bluesound/add album)))

(defun bluesound/presets ()
  "Return an alist of presets."
  (mapcar (lambda (n)
            (cons (bluesound/-attr 'name n)
                  (bluesound/-attr 'id n)))
          (bluesound/-query-all '(presets preset)
                                (bluesound/-GET "Presets"))))

;;;###autoload
(defun bluesound/preset (preset)
  "Play preset named PRESET."
  (interactive
   (list
    (completing-read "Preset: "
                     (mapcar #'car (bluesound/presets)))))
  (when preset
    (when-let (id (cdr (assoc preset (bluesound/presets))))
      (bluesound/-GET (concat "Preset?id=" id))
      (message "Playing: %s" preset))))

;;;###autoload
(defun bluesound/next ()
  "Play next in queue."
  (interactive)
  (bluesound/-GET "Skip"))

;;;###autoload
(defun bluesound/previous ()
  "Play previous in queue."
  (interactive)
  (bluesound/-GET "Back"))

;;;###autoload
(defun bluesound/current ()
  "Show what's playing now."
  (interactive)
  (let* ((status (bluesound/status))
         (title (string-join
                 (seq-filter #'stringp
                             (list (cdr (assoc 'title2 status))
                                   (cdr (assoc 'title3 status))
                                   (cdr (assoc 'title1 status))))
                 "  /  ")))
    (message "Playing: %s" title)
    title))

(provide 'bluesound)

;;; bluesound.el ends here