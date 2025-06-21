(ns romaji-to-hiragana.core
  (:require [clojure.string :as str]))

(def romaji-hiragana-map
  {"a" "あ", "i" "い", "u" "う", "e" "え", "o" "お",
   "ka" "か", "ki" "き", "ku" "く", "ke" "け", "ko" "こ",
   "sa" "さ", "shi" "し", "su" "す", "se" "せ", "so" "そ",
   "ta" "た", "chi" "ち", "tsu" "つ", "te" "て", "to" "と",
   "na" "な", "ni" "に", "nu" "ぬ", "ne" "ね", "no" "の",
   "ha" "は", "hi" "ひ", "fu" "ふ", "he" "へ", "ho" "ほ",
   "ma" "ま", "mi" "み", "mu" "む", "me" "め", "mo" "も",
   "ya" "や", "yu" "ゆ", "yo" "よ",
   "ra" "ら", "ri" "り", "ru" "る", "re" "れ", "ro" "ろ",
   "wa" "わ", "wo" "を", "n" "ん",
   "ga" "が", "gi" "ぎ", "gu" "ぐ", "ge" "げ", "go" "ご",
   "za" "ざ", "ji" "じ", "zu" "ず", "ze" "ぜ", "zo" "ぞ",
   "da" "だ", "de" "で", "do" "ど",
   "ba" "ば", "bi" "び", "bu" "ぶ", "be" "べ", "bo" "ぼ",
   "pa" "ぱ", "pi" "ぴ", "pu" "ぷ", "pe" "ぺ", "po" "ぽ",
   "kya" "きゃ", "kyu" "きゅ", "kyo" "きょ",
   "sha" "しゃ", "shu" "しゅ", "sho" "しょ",
   "cha" "ちゃ", "chu" "ちゅ", "cho" "ちょ",
   "nya" "にゃ", "nyu" "にゅ", "nyo" "にょ",
   "hya" "ひゃ", "hyu" "ひゅ", "hyo" "ひょ",
   "mya" "みゃ", "myu" "みゅ", "myo" "みょ",
   "rya" "りゃ", "ryu" "りゅ", "ryo" "りょ",
   "gya" "ぎゃ", "gyu" "ぎゅ", "gyo" "ぎょ",
   "ja" "じゃ", "ju" "じゅ", "jo" "じょ",
   "bya" "びゃ", "byu" "びゅ", "byo" "びょ",
   "pya" "ぴゃ", "pyu" "ぴゅ", "pyo" "ぴょ"})

(def particle-map
  {"wa" "は"
   "o" "を"
   "e" "へ"})

(defn preprocess-special-particles [s]
  (let [particle-regex #"(^|\s|[.,!?;:])(wa|o|e)(\s|$|[.,!?;:])"]
    (str/replace s particle-regex
                 (fn [[_ pre particle post]]
                   (str pre (particle-map particle) post)))))
(defn preprocess-double-consonants [s]
  (-> s
      (str/replace #"([kgsztdjpbc])\1" "っ$1")
      (str/replace #"n([^aiueoyn]|$)" "ん$1")))

(defn romaji-to-hiragana [input]
  (let [s (str/lower-case input)
        preprocessed (-> s
                         preprocess-special-particles
                         preprocess-double-consonants)
        sorted-keys (sort-by (comp - count) (keys romaji-hiragana-map))]
    (loop [remaining preprocessed
           result []]
      (if (empty? remaining)
        (str/join result)
        (let [match (some (fn [k]
                            (when (str/starts-with? remaining k)
                              [k (romaji-hiragana-map k)]))
                          sorted-keys)]
          (if match
            (recur (subs remaining (count (first match)))
                   (conj result (second match)))
            (recur (subs remaining 1)
                   (conj result (subs remaining 0 1)))))))))

(defn -main [& args]
  (if (empty? args)
    (println (romaji-to-hiragana (first args)))))
