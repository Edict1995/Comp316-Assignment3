(ns assignment3.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
)

(defn new-image
  "Function to create a new image."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
  )

(defn read-image
  "Function to read an image from a file."
  [filename]
    (let [file (File. filename)]
      (ImageIO/read file)
    )
)

(defn save-image
  "Function to save an image with a particular extension to a file."
  [image extension filename]
    (let [file (File. filename)]
      (ImageIO/write image extension file)
    )
)

(defn get-width
  "Function to get the width of an image."
  [image]
  (.getWidth image)
)

(defn get-height
  "Function to get the height of an image."
  [image]
  (.getHeight image)
  )

(defn get-rgb
  "Function to get the RGB components of a pixel in a vector of length 3."
  [image x y]
    (let [rgb (.getRGB image x y)
          red (bit-shift-right (bit-and rgb 0xFF0000) 16)
          blue (bit-shift-right (bit-and rgb 0xFF00) 8)
          green (bit-and rgb 0xFF)
          ]
        (vec (list red green blue))
      )
)

(defn set-rgb
  "Function to set the RGB components of a pixel."
  [image x y [red green blue]]
     (let [rgb (+ (bit-shift-left red 16)
                  (bit-shift-left blue 8)
                  (bit-shift-left green 0) ) ]
       (.setRGB image x y rgb)
   )
  )

(defn get-grey
  "Function to get the grey vaule of pixel"
  [image x y]
  (let [result (get-rgb image x y)]
    (/ (reduce + result) 3)
    )
  )

(defn get-grey-in-bounds
  "Function to get the grey value or the hotspot if it is not in bounds."
  [image x y hotspot]
  (let [width (get-width image)
        height (get-height image)]
    (if (< x 0)
      hotspot
      (if (>= x width)
        hotspot
        (if (< y 0)
          hotspot
          (if (>= y height)
            hotspot
            (get-grey image x y)))))))

(defn set-grey
  "Function to set the grey value of a pixel."
  [image x y grey]
   (set-rgb image x y [grey grey grey])
)

(defn -main
  "Test the image functions."
  [& args]
  
)

(defn  create-matrix
  "Function  to  create a matrix  of zeros."
  [rows  cols]
  (vec
   (repeat  rows
            (vec (repeat  cols  0)))
   )
  )

(defn get-dims-matrix
  [matrix]
  {:nrows (count  matrix)
   :ncols (count (first  matrix ))}
  )

(defn  get-matrix
  "Function  to get the  value  from a matrix."
  [matrix  row col]
  (nth (nth  matrix  row) col)
  )

(defn  set-matrix
  "Function  to set a value  in a matrix. Returns  the  modified  matrix."
  [matrix  row col  value]
  (assoc  matrix  row
          (assoc (nth  matrix  row)
                 col  value))
  )

(defn  print-matrix
  "Function  to print a matrix  tidily."
  [matrix]
  (doseq [row  matrix]
    (doseq [value  row]
      (print  value "\t"))
    (println)
    ))

(defn  get-matrix-row
  "Function  to get a matrix  row."
  [matrix  row]
  (nth  matrix  row)
  )

(defn  get-matrix-col
  "Function  to get a matrix  column."
  [matrix  col]
  (vec (map #(nth % col) matrix ))
  )

(defn kirsch
  "Ables krish filter in direction i from given file"
  [filename i]
  (let [image (read-image filename)
        width (get-width image)
        height (get-height image)
        new-filtered-bad-boi (new-image width height )]
    (dotimes [x width]
      (dotimes [y height]
        (set-grey new-filtered-bad-boi
                  x y
                  (filter (get-neighbours x y image)
                          (filters i)))))
    (save-image new-filtered-bad-boi "png" (str filename "superBABBOI"))
    )
  )

(kirsch "/home/sam/assignment3/resources/car1.jpg" 0)
  


(defn get-neighbours
  "Takes a hotspot and gets the pixels around it"
  [x y file]
  (let [top1-coord [(- x 1)(- y 1)]
        top2-coord [ x (- y 1)]
        top3-coord [(+ x 1)(- y 1)]
        mid1-coord [(- x 1) y ]
        hotspot (get-grey file x y)
        mid3-coord [(+ x 1) y]
        bot1-coord [(- x 1)(+ y 1)]
        bot2-coord [ x (+ y 1)]
        bot3-coord [(+ x 1)(+ y 1)]
        initial-matrix (create-matrix 3 3)
        modified-matrix (-> (set-matrix initial-matrix 0 0 (get-grey-in-bounds file (first top1-coord) (last top1-coord) hotspot))
                            (set-matrix 0 1 (get-grey-in-bounds file (first top2-coord) (last top2-coord) hotspot))
                            (set-matrix 0 2 (get-grey-in-bounds file (first top3-coord) (last top3-coord) hotspot))
                            (set-matrix 1 0 (get-grey-in-bounds file (first mid1-coord) (last mid1-coord) hotspot))
                            (set-matrix 1 1 hotspot)
                            (set-matrix 1 2 (get-grey-in-bounds file (first mid3-coord) (last mid3-coord) hotspot))
                            (set-matrix 2 0 (get-grey-in-bounds file (first bot1-coord) (last bot1-coord) hotspot))
                            (set-matrix 2 1 (get-grey-in-bounds file (first bot2-coord) (last bot2-coord) hotspot))
                            (set-matrix 2 2 (get-grey-in-bounds file (first bot3-coord) (last bot3-coord) hotspot))
                            )
        ]
    modified-matrix
    )
  )
;; (get-neighbours 2 3 (read-image "/home/sam/assignment3/resources/car1.jpg"))
;; (get-neighbours 0 0 (read-image "/home/sam/assignment3/resources/car1.jpg"))

(defn multiply-matrix
  "Takes two matrices and times them together."
  [matrix1 matrix2]
  (mapv #(mapv * %1 %2) matrix1 matrix2)
  )
;; (multiply-matrix [[1 1 1][2 2 2]] [[3 3 3][4 4 4]])

(defn sum-vector
  "Takes a vector and returns it sum"
  [vector]
  (reduce + vector))
;; (sum-vector [1 2 3])

(defn sum-matrix
  "Takes a matrix and returns the sum"
  [matrix]
  (reduce + (flatten matrix)))
;; (sum-matrix [[1 1 1][2 2 2][3 3 3]])

(defn filter
  "Takes a filter and a pixel a matrix and applies the filter."
  [filter matrix]
  (+ 127 (Math/round (float (/ (sum-matrix (multiply-matrix filter matrix)) 8)))))
;; (filter (filters 0) (get-neighbours 0 0 (read-image "/home/sam/assignment3/resources/car1.jpg")))

(defn filters
  "Returns 1 of 9 filters depending on the direction you give it."
  [direction]
  (let [kirsch-filter [[[-1 0 1][-2 0 2][-1 0 1]]
                       [[-2 -1 0][-1 0 1][0 1 2]]
                       [[-1 -2 -1][0 0 0][1 2 1]]
                       [[0 -1 -2][1 0 -1][2 1 0]]
                       [[1 0 -1][2 0 -2][1 0 -1]]
                       [[2  1 0][1 0 -1][0 -1 -2]]
                       [[1 2 1][0 0 0][-1 -2 -1]]                                                                                                                                                                                                                     [[][][]]
                       [[0 1 2][-1 0 1][-2 -1 0]]]]
    (nth kirsch-filter direction)))
;; (filters 2)
