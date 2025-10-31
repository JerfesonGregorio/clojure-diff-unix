(ns clojure-diff-unix.core
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:gen-class))

(def base-path (System/getProperty "user.dir"))

(defn executar-diff
  "Executa 'diff -u' entre dois arquivos e retorna um mapa semântico.
   Retorna:
   - {:status :identical}
   - {:status :different, :diff-output \"...\"}
   - {:status :error, :message \"...\"}"
  [file1 file2]

  (let [comando (str "diff -u <(nl " file1 ") <(nl " file2 ")")
        resultado (shell/sh "bash" "-c" comando)]
    (case (:exit resultado)
      ;; Idênticos
      0 {:status :identical}

      ;; Diferentes
      1 {:status :different
         :diff-output (:out resultado)}

      {:status :error
       :message (:err resultado)
       :exit-code (:exit resultado)})))

(defn parse-unified-diff
  "Converte o conteúdo de um 'diff -u' (como string)
  para um mapa com :deletado, :alterado, e :adicionado."
  [unified-diff-content]
  (let [linhas (str/split-lines unified-diff-content)]

    (loop [linhas-restantes linhas
           deletado-buffer []
           adicionado-buffer []
           acc {:deletado [] :alterado [] :adicionado []}]

      (let [linha (first linhas-restantes)
            proximas-linhas (rest linhas-restantes)]

        (cond
          (nil? linha)
          (cond
            (and (seq deletado-buffer) (seq adicionado-buffer))
            (update acc :alterado concat (concat deletado-buffer adicionado-buffer))

            (seq deletado-buffer)
            (update acc :deletado concat deletado-buffer)

            (seq adicionado-buffer)
            (update acc :adicionado concat adicionado-buffer)

            :else acc)

          (or (str/starts-with? linha "---")
              (str/starts-with? linha "+++"))
          (recur proximas-linhas deletado-buffer adicionado-buffer acc)

          (or (str/starts-with? linha " ")
              (str/starts-with? linha "@@"))
          (let [
                novo-acc (cond
                           (and (seq deletado-buffer) (seq adicionado-buffer))
                           (update acc :alterado concat (concat deletado-buffer adicionado-buffer))

                           (seq deletado-buffer)
                           (update acc :deletado concat deletado-buffer)

                           (seq adicionado-buffer)
                           (update acc :adicionado concat adicionado-buffer)

                           :else acc)]

            (recur proximas-linhas [] [] novo-acc))

          (str/starts-with? linha "-")
          (recur proximas-linhas
                 (conj deletado-buffer linha)
                 adicionado-buffer
                 acc)

          (str/starts-with? linha "+")
          (recur proximas-linhas
                 deletado-buffer
                 (conj adicionado-buffer linha)
                 acc)

          :else
          (recur proximas-linhas deletado-buffer adicionado-buffer acc))))))


(defn formatar-linha-simples [linha]

  (let [conteudo (str/triml (subs linha 1))]
    (str/replace-first conteudo #"\t" " - ")))

(defn formatar-bloco-alterado [bloco-alterado]
  (let [pares (partition 2 bloco-alterado)]
    (map (fn [[linha-antiga linha-nova]]
           (let [conteudo-antigo (str/triml (subs linha-antiga 1))
                 conteudo-novo (str/triml (subs linha-nova 1))

                 numero-linha (first (str/split conteudo-novo #"\t"))

                 dado-antigo (second (str/split conteudo-antigo #"\t" 2))
                 dado-novo (second (str/split conteudo-novo #"\t" 2))]

             (str numero-linha " - " dado-antigo " ** " dado-novo)))
         pares)))

(defn formatar-para-custom
  "Recebe o mapa do 'parse-unified-diff' e formata na saída '### ... ###'."
  [mapa-diff]
  (let [placeholder "----------------------------------------"
        deletados (if (seq (:deletado mapa-diff))
                    (map formatar-linha-simples (:deletado mapa-diff))
                    ; else
                    [placeholder])
        alterados (if (seq (:alterado mapa-diff))
                    (formatar-bloco-alterado (:alterado mapa-diff))
                    ; else
                    [placeholder])
        adicionados (if (seq (:adicionado mapa-diff))
                      (map formatar-linha-simples (:adicionado mapa-diff))
                      ; else
                      [placeholder])]
    (str
      "### Deletado ###\n"
      (str/join "\n" deletados) "\n\n"
      "### Alterado ###\n"
      (str/join "\n" alterados) "\n\n"
      "### Adicionado ###\n"
      (str/join "\n" adicionados) "\n")))

(def diff (:diff-output (executar-diff (str base-path "/resources/SPED-FISCAL-FIS-E_-_7595331.txt")
                                       (str base-path "/resources/CAT-422018-geração--cenário-1|DEVELOPMENT.txt"))))
(def diff-mapeado (parse-unified-diff diff))
(def saida-final (formatar-para-custom diff-mapeado))


(def d (:diff-output (executar-diff (str base-path "/resources/original.txt")
                                    (str base-path "/resources/replace.txt"))))

(def d-mapeado (parse-unified-diff d))
(def saida (formatar-para-custom d-mapeado))

(spit "resources/diff-replace.txt" saida)

(defn -main
  [& args]
  (println "Hello, World!"))
