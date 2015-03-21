(ns rna-transcription)

(defn to-rna [rna-str]
  (letfn [(translate [input]
            (assert (#{\G \C \U \A \T} input))
            ({\G "C"
              \C "G"
              \U "A"
              \A "U"
              \T "A"}
             input))]
    (apply str (map translate rna-str))))