; This script was last used with Clojure 1.0

(use 'clojure.contrib.duck-streams)
(use 'clojure.contrib.str-utils)

(def stop-re #"\.")

(def gismu-lines (rest (read-lines "gismu.txt")))
(def cmavo-lines (rest (read-lines "cmavo.txt")))

(defstruct word-s :type :word :rafsi :selmaho :keyword :hint :definition :textbook :frequency
                  :misc-info)
(def get-type (accessor word-s :type))
(def get-selmaho (accessor word-s :selmaho))
(def get-word (accessor word-s :word))
(def get-keyword (accessor word-s :keyword))
(def get-definition (accessor word-s :definition))
(def get-frequency (accessor word-s :frequency))
(def get-misc-info (accessor word-s :misc-info))

(def xml-escaped-chars
  {#"<" "&lt;"
   #">" "&gt;"
   #"&" "&amp;"
   #"'" "&apos;"
   #"\"" "&quot;"})

(def id-escaped-chars
  {#"&apos;" "h"
   #"\." "_"
   #"\s" "-"})

(defn sub-escape-chars [escaped-chars string]
  (loop [cur-string string, cur-escaped-char-seq escaped-chars]
    (if-not (empty? cur-escaped-char-seq)
      (let [[pattern replacement] (first cur-escaped-char-seq)]
        (recur (re-gsub pattern replacement cur-string)
               (rest cur-escaped-char-seq)))
      cur-string)))

(def sub-xml-escape-chars (partial sub-escape-chars xml-escaped-chars))
(def sub-id-escape-chars (partial sub-escape-chars id-escaped-chars))

(def gismu-columns [[:word 0 6] [:rafsi 7 19] [:keyword 20 39] [:hint 41 60]
                    [:definition 62 157] [:textbook 160 162] [:frequency 163 164]
                    [:misc-info 165 nil]])

(def cmavo-columns [[:word 0 10] [:selmaho 11 19] [:keyword 20 61] [:definition 62 167]
                    [:misc-info 168 nil]])

(defn parse-data [[line-seq column-limits word-type]]
  (into {}
    (map #(vector (sub-id-escape-chars (get-word %)) %)
      (for [line line-seq]
        (let [line-length (count line)]
          (apply struct-map word-s
            (concat [:type word-type]
              (apply concat
                (for [[key l-column r-column] column-limits :when (< l-column line-length)]
                  [key
                   (sub-xml-escape-chars (.trim (if (and r-column (< r-column line-length))
                                                  (subs line l-column r-column)
                                                  (subs line l-column))))])))))))))

(def word-data
  (apply merge (map parse-data [[gismu-lines gismu-columns "gismu"]
                                [cmavo-lines cmavo-columns "cmavo"]])))

(defn transform-string [string process]
  (if (empty? string) "" (process string)))

(def split-definitions vector)
;(def split-definitions (partial re-split #"\s*;\s*"))
(def sub-definition-vars (partial re-gsub #"(x\d)" (fn [[_ variable]]
                                                      (str "<var>" variable "</var>"))))
(def transform-definitions (partial map (partial format "<li>%s</li>")))
(def join-definitions (partial str-join "\n"))
(def remove-bad-indexes (partial remove #(or (nil? %) (= "the" %))))
(def transform-indexes (partial map (partial format "<d:index d:value=\"%s\"/>")))

(defn- prepare-indexes [word keyword]
  (let [stripped-word (re-gsub stop-re "" word)
        keyword-tokens (re-split #"\s+" keyword)]
    (-> #{word stripped-word keyword} (into keyword-tokens) remove-bad-indexes
        transform-indexes join-definitions)))

(defn- prepare-definition [string]
  (-> string sub-definition-vars split-definitions transform-definitions join-definitions))

(defn- prepare-misc-info [string]
  (transform-string string (partial format "<p class=\"note\">%s</p>")))

(defn dump-xml [data]
  (println "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  (println "<d:dictionary xmlns=\"http://www.w3.org/1999/xhtml\"")
  (println "  xmlns:d=\"http://www.apple.com/DTDs/DictionaryService-1.0.rng\">\n")
  
  (println "<d:entry id=\"front-back-matter\" d:title=\"(front/back matter)\">")
  (println "<h1>The Lojban Dictionary in English</h1>")
  (println "<p>Based on the word lists from the Logical Language Group of the 1990s</p>")
  (println "</d:entry>")
  
  (doseq [[word-id word-datum] data]
    (let [type (get-type word-datum)
          word (get-word word-datum)
          keyword (get-keyword word-datum)
          definition (get-definition word-datum)
          frequency (get-frequency word-datum)
          misc-info (get-misc-info word-datum)]
      (printf "<d:entry id=\"%s\" d:title=\"%s\">

%s
<h1>%s</h1>
<p class=\"word-type\">%s</p>
<ul>
%s
</ul>
%s<p class=\"minor-note\">Frequency: %s</p>

</d:entry>
"
        word-id word (prepare-indexes word keyword) word type (prepare-definition definition)
        (prepare-misc-info misc-info) frequency)))
  (println "</d:dictionary>"))

(dump-xml word-data)














