;;;; src/server/check-page.lisp
;;;;
;;;; Проверочная страница этапа 0. Показывает, что сервер в виртуалке
;;;; виден из браузера хоста через проброс порта, и сразу проверяет, есть ли
;;;; в этом браузере то, на что опирается просмотрщик: WebGL2, WebSocket,
;;;; DataView с 64-битными целыми, Blob.slice.

(in-package #:worm.server)

(defvar *default-port* 8740
  "Порт сервера по умолчанию (архитектура, раздел «Среда исполнения»).")

(defvar *acceptor* nil
  "Запущенный акцептор Hunchentoot или NIL.")

(defun html-escape (string)
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char ch out))))))

(defun timestamp-string ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D UTC" y mo d h mi s)))

(defun server-facts ()
  "Пары (подпись . значение) о процессе, отдающем страницу."
  (let ((rev (worm.core:git-revision)))
    (list (cons "Реализация" (worm.platform:implementation-description))
          (cons "Ревизия" (if rev (subseq rev 0 (min 12 (length rev))) "нет данных"))
          (cons "Динамическая память" (format nil "~D МБ" (worm.platform:dynamic-space-megabytes)))
          (cons "Ядер" (format nil "~D" (worm.platform:processor-count)))
          (cons "Время сервера" (timestamp-string)))))

(defparameter *browser-check-script* "
(function () {
  var rows = [];
  function add(name, ok, note) { rows.push([name, ok, note || '']); }
  var gl = null;
  try { gl = document.createElement('canvas').getContext('webgl2'); } catch (e) {}
  add('WebGL2', !!gl, gl ? gl.getParameter(gl.VERSION) : 'нет контекста');
  add('WebSocket', typeof WebSocket === 'function');
  var dv = typeof DataView === 'function' && typeof DataView.prototype.getBigUint64 === 'function';
  add('DataView + BigUint64', dv);
  add('Blob.slice', typeof Blob === 'function' && typeof Blob.prototype.slice === 'function');
  var tbody = document.getElementById('browser');
  rows.forEach(function (r) {
    var tr = document.createElement('tr');
    tr.innerHTML = '<th></th><td class=\"' + (r[1] ? 'ok' : 'bad') + '\"></td><td class=\"note\"></td>';
    tr.children[0].textContent = r[0];
    tr.children[1].textContent = r[1] ? 'есть' : 'нет';
    tr.children[2].textContent = r[2];
    tbody.appendChild(tr);
  });
  document.getElementById('ua').textContent = navigator.userAgent;
})();
")

(defun check-page-html ()
  (with-output-to-string (out)
    (format out "<!doctype html>
<html lang=\"ru\"><head><meta charset=\"utf-8\">
<title>Wormlisp — проверка связи</title>
<style>
 body { margin: 0; padding: 32px; background: #f4f1ea; color: #1d2421;
        font: 15px/1.5 \"DejaVu Sans\", sans-serif; }
 main { max-width: 640px; }
 h1 { font-size: 22px; margin: 0 0 4px; }
 p.lead { margin: 0 0 24px; color: #4d5852; }
 h2 { font-size: 14px; text-transform: uppercase; letter-spacing: .06em;
      color: #4d5852; margin: 24px 0 8px; }
 table { border-collapse: collapse; width: 100%; }
 th, td { text-align: left; padding: 6px 12px 6px 0; border-bottom: 1px solid #ddd6c8;
          vertical-align: top; }
 th { font-weight: 600; width: 40%; }
 td.ok { color: #24704a; font-weight: 600; }
 td.bad { color: #a8322d; font-weight: 600; }
 td.note, #ua { color: #4d5852; font-size: 13px; }
</style></head><body><main>
<h1>Wormlisp на связи</h1>
<p class=\"lead\">Сервер процесса REPL отвечает. Этап 0: проверка связи виртуалки и браузера хоста.</p>
<h2>Сервер</h2><table>")
    (loop for (label . value) in (server-facts)
          do (format out "<tr><th>~A</th><td>~A</td></tr>~%"
                     (html-escape label) (html-escape value)))
    (format out "</table>
<h2>Браузер</h2><table><tbody id=\"browser\"></tbody></table>
<p id=\"ua\"></p>
<script>~A</script>
</main></body></html>" *browser-check-script*)))

(hunchentoot:define-easy-handler (check-page :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (check-page-html))

(hunchentoot:define-easy-handler (health :uri "/health") ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "ok")

(defun log-path ()
  (let ((dir (merge-pathnames "log/" (worm.core:project-root))))
    (ensure-directories-exist dir)
    (merge-pathnames "server.log" dir)))

(defun serving-p ()
  (and *acceptor* (hunchentoot:started-p *acceptor*)))

(defun serve (&key (port *default-port*) (address nil))
  "Запустить HTTP-сервер на PORT. ADDRESS NIL — все интерфейсы виртуалки
\(нужно для проброса порта). Повторный вызов перезапускает сервер."
  (when (serving-p) (stop-serving))
  (setf hunchentoot:*hunchentoot-default-external-format* :utf-8
        hunchentoot:*default-content-type* "text/html; charset=utf-8")
  (setf *acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port port
                       :address address
                       :access-log-destination nil
                       :message-log-destination (log-path)))
  (hunchentoot:start *acceptor*)
  (format t "~&Сервер: http://localhost:~D/  (журнал: ~A)~%"
          (hunchentoot:acceptor-port *acceptor*) (namestring (log-path)))
  *acceptor*)

(defun stop-serving ()
  "Остановить HTTP-сервер, если он запущен."
  (when *acceptor*
    (when (hunchentoot:started-p *acceptor*)
      (hunchentoot:stop *acceptor* :soft t))
    (setf *acceptor* nil))
  t)
