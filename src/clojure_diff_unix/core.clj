(ns clojure-diff-unix.core
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [editscript.core :as e-diff])
  (:gen-class))

(def base-path (System/getProperty "user.dir"))

(defn executar-diff
  "Executa 'wdiff' entre dois arquivos e retorna um mapa semântico.
   Retorna:
   - {:status :identical}
   - {:status :different, :diff-output \"...\"}
   - {:status :error, :message \"...\"}"
  [file1 file2]
  (let [comando (str "wdiff <(nl " file1 ") <(nl " file2 ")")
        resultado (shell/sh "bash" "-c" comando)]
    (case (:exit resultado)
      ;; Idênticos
      0 {:status :identical}

      ;; Diferentes
      1 {:status :different
         :diff-output (:out resultado)}

      ;; Erro
      {:status :error
       :message (:err resultado)
       :exit-code (:exit resultado)})))


(def cat1 "/resources/cat1.txt")
(def cat2 "/resources/cat2.txt")


(def diff (:diff-output (executar-diff (str base-path cat1)
                                       (str base-path cat2))


            ))







(defn -main
  [& args]
  (println "Hello, World!"))
