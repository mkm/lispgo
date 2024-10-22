(in-package #:euler)

(declaim (optimize (speed 1)))
(setf *print-circle* t)

(declaim (inline sqr))
(defun sqr (x)
  (declare (type integer x))
  (* x x))

(defun fact (n)
  (loop :with r = 1
        :for k :from 2 :to n
        :do (setf r (* r k))
        :finally (return r)))

(defun binom (n k)
  (/ (fact n) (* (fact k) (fact (- n k)))))

(defun divp (n d)
  (declare (type integer n d))
  (= (rem n d) 0))

(defun factors (n)
  (declare (type integer n))
  (if (= n 0)
      nil
      (if (< n 0)
          (cons -1 (factors (- n)))
          (loop :with d = 2
                :when (divp n d)
                :collect d :into result
                :and :do (setf n (/ n d))
                :else
                :when (> (sqr d) n)
                :when (> n 1) :collect n :into result :end
                :and :do (return result)
                :else
                :do (incf d)))))

(defvar *primep-cache* (make-hash-table))

(defun primep (n)
  (or (gethash n *primep-cache*)
      (setf (gethash n *primep-cache*)
            (= (length (factors n)) 1))))

(defun prime-power-p (n)
  (apply #'= (factors n)))

(defun num-divisors (n)
  (declare (type integer n))
  (loop :with c = 0
        :for d :of-type integer :from 1
        :while (<= (sqr d) n)
        :when (divp n d)
        :do (incf c 2)
        :when (= (sqr d) n)
        :do (decf c 1)
        :finally (return c)))

(defvar *sum-divisors-cache* (make-hash-table))

(defun sum-divisors (n)
  (or (gethash n *sum-divisors-cache*)
      (loop :for d :from 1
            :while (<= (sqr d) n)
            :when (= (sqr d) n)
            :sum d :into result
            :else
            :when (divp n d)
            :sum (+ (/ n d) d) :into result
            :finally
            (return (setf (gethash n *sum-divisors-cache*) result)))))

(defun sum-proper-divisors (n)
  (- (sum-divisors n) n))

(defun divisors (n)
  (loop :for d :from 1
        :while (<= (sqr d) n)
        :when (= (sqr d) n)
        :collect d
        :else
        :when (divp n d)
        :append (list (/ n d) d)))

(defun proper-divisors (n)
  (cdr (divisors n)))

(defun abundantp (n)
  (> (sum-proper-divisors n) n))

(defun palindrome-p (str)
  (equalp str (reverse str)))

(defun digit (c)
  (- (char-code c) (char-code #\0)))

(defun remove-at (index seq &key (count 1))
  (remove-if (constantly t) seq :start index :count count))

(defun nth-permutation (n elems)
  (if (null (cdr elems))
      elems
      (let* ((period (fact (- (length elems) 1)))
             (index (floor (/ n period))))
        (cons (nth index elems)
              (nth-permutation (rem n period) (remove-at index elems))))))

(defun maximum (lessp elems)
  (loop :with best-elem = (car elems)
        :for elem :in (cdr elems)
        :do (when (funcall lessp best-elem elem)
              (setf best-elem elem))
        :finally (return best-elem)))

(defvar *decimal-repr* (make-hash-table))

(defun decimal-repr (q)
  (if (= q 0)
      nil
      (or (gethash q *decimal-repr*)
          (multiple-value-bind (digit r) (floor q 1/10)
            (let ((digits (setf (gethash q *decimal-repr*) (list digit))))
              (setf (cdr digits) (decimal-repr (* r 10)))
              digits)))))

(defun find-cycle (elems)
  (loop :for x :on elems :by #'cdr
        :for y :on (cdr elems) :by #'cddr
        :when (eq x y)
        :do (return x)))

(defun cycle-length (cycle)
  (if (null cycle)
      0
      (loop :for tail :on (cdr cycle) :by #'cdr
            :for len :from 1
            :when (eq tail cycle)
            :do (return len))))

(defun problem1 ()
  (loop :for n :from 0 :below 1000
        :when (or (divp n 3)
                  (divp n 5))
        :sum n))

(defun problem2 ()
  (loop :with n = 1
        :and k = 0
        :while (< n 4000000)
        :do (setf (values n k) (values (+ n k) n))
        :when (evenp n) :sum n))

(defun problem3 ()
  (car (last (factors 600851475143))))

(defun problem4 ()
  (loop :for m :from 100 :below 1000
        :maximize (loop :for n :from 100 :below 1000
                        :when (palindrome-p (write-to-string (* m n)))
                        :maximize (* m n))))

(defun problem5 ()
  (* 5 7 9 11 13 16 17 19))

(defun problem6 ()
  (- (sqr (reduce #'+ (iota 101)))
     (reduce #'+ (map 'list #'sqr (iota 101)))))

(defun problem7 ()
  (loop :for n :from 2
        :count (primep n) :into c
        :when (= c 10001)
        :do (return n)))

(defparameter *input8* "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defun problem8 ()
  (loop :for i :from 0 :below (- (length *input8*) 12)
        :maximize (reduce #'* (map 'list #'digit (subseq *input8* i (+ i 13))))))

(defun problem9 ()
  (loop :for a :from 0 :below 333
        :do (loop :for b :from 0 :below 500
                  :when (= (+ (sqr a) (sqr b)) (sqr (- 1000 a b)))
                  :do (return-from problem9 (* a b (- 1000 a b))))))

(defun problem10 ()
  (loop :for n :from 0 :below 2000000
        :when (primep n)
        :sum n))

(defparameter *input11*
  #2A((08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
      (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
      (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
      (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
      (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
      (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
      (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
      (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
      (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
      (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
      (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
      (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
      (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
      (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
      (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
      (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
      (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
      (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
      (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
      (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))

(defun problem11 ()
  (flet ((index (col row) (handler-case (aref *input11* col row) (t () 0))))
    (loop :for row :from 0 :below 20
          :maximize
          (loop :for col :from 0 :below 20
                :maximize
                (loop :for (r c) :in '((1 0) (0 1) (-1 0) (0 -1) (1 1) (1 -1))
                      :maximize
                      (reduce #'*
                              (loop :for i :from 0 :to 3
                                    :collect (index (+ row (* r i)) (+ col (* c i))))))))))

(defun problem12 ()
  (loop :with n = 0
        :for k :from 0
        :do (incf n k)
        :when (> (num-divisors n) 500)
        :do (return n)))

(defparameter *input13*
  '(37107287533902102798797998220837590246510135740250
    46376937677490009712648124896970078050417018260538
    74324986199524741059474233309513058123726617309629
    91942213363574161572522430563301811072406154908250
    23067588207539346171171980310421047513778063246676
    89261670696623633820136378418383684178734361726757
    28112879812849979408065481931592621691275889832738
    44274228917432520321923589422876796487670272189318
    47451445736001306439091167216856844588711603153276
    70386486105843025439939619828917593665686757934951
    62176457141856560629502157223196586755079324193331
    64906352462741904929101432445813822663347944758178
    92575867718337217661963751590579239728245598838407
    58203565325359399008402633568948830189458628227828
    80181199384826282014278194139940567587151170094390
    35398664372827112653829987240784473053190104293586
    86515506006295864861532075273371959191420517255829
    71693888707715466499115593487603532921714970056938
    54370070576826684624621495650076471787294438377604
    53282654108756828443191190634694037855217779295145
    36123272525000296071075082563815656710885258350721
    45876576172410976447339110607218265236877223636045
    17423706905851860660448207621209813287860733969412
    81142660418086830619328460811191061556940512689692
    51934325451728388641918047049293215058642563049483
    62467221648435076201727918039944693004732956340691
    15732444386908125794514089057706229429197107928209
    55037687525678773091862540744969844508330393682126
    18336384825330154686196124348767681297534375946515
    80386287592878490201521685554828717201219257766954
    78182833757993103614740356856449095527097864797581
    16726320100436897842553539920931837441497806860984
    48403098129077791799088218795327364475675590848030
    87086987551392711854517078544161852424320693150332
    59959406895756536782107074926966537676326235447210
    69793950679652694742597709739166693763042633987085
    41052684708299085211399427365734116182760315001271
    65378607361501080857009149939512557028198746004375
    35829035317434717326932123578154982629742552737307
    94953759765105305946966067683156574377167401875275
    88902802571733229619176668713819931811048770190271
    25267680276078003013678680992525463401061632866526
    36270218540497705585629946580636237993140746255962
    24074486908231174977792365466257246923322810917141
    91430288197103288597806669760892938638285025333403
    34413065578016127815921815005561868836468420090470
    23053081172816430487623791969842487255036638784583
    11487696932154902810424020138335124462181441773470
    63783299490636259666498587618221225225512486764533
    67720186971698544312419572409913959008952310058822
    95548255300263520781532296796249481641953868218774
    76085327132285723110424803456124867697064507995236
    37774242535411291684276865538926205024910326572967
    23701913275725675285653248258265463092207058596522
    29798860272258331913126375147341994889534765745501
    18495701454879288984856827726077713721403798879715
    38298203783031473527721580348144513491373226651381
    34829543829199918180278916522431027392251122869539
    40957953066405232632538044100059654939159879593635
    29746152185502371307642255121183693803580388584903
    41698116222072977186158236678424689157993532961922
    62467957194401269043877107275048102390895523597457
    23189706772547915061505504953922979530901129967519
    86188088225875314529584099251203829009407770775672
    11306739708304724483816533873502340845647058077308
    82959174767140363198008187129011875491310547126581
    97623331044818386269515456334926366572897563400500
    42846280183517070527831839425882145521227251250327
    55121603546981200581762165212827652751691296897789
    32238195734329339946437501907836945765883352399886
    75506164965184775180738168837861091527357929701337
    62177842752192623401942399639168044983993173312731
    32924185707147349566916674687634660915035914677504
    99518671430235219628894890102423325116913619626622
    73267460800591547471830798392868535206946944540724
    76841822524674417161514036427982273348055556214818
    97142617910342598647204516893989422179826088076852
    87783646182799346313767754307809363333018982642090
    10848802521674670883215120185883543223812876952786
    71329612474782464538636993009049310363619763878039
    62184073572399794223406235393808339651327408011116
    66627891981488087797941876876144230030984490851411
    60661826293682836764744779239180335110989069790714
    85786944089552990653640447425576083659976645795096
    66024396409905389607120198219976047599490197230297
    64913982680032973156037120041377903785566085089252
    16730939319872750275468906903707539413042652315011
    94809377245048795150954100921645863754710598436791
    78639167021187492431995700641917969777599028300699
    15368713711936614952811305876380278410754449733078
    40789923115535562561142322423255033685442488917353
    44889911501440648020369068063960672322193204149535
    41503128880339536053299340368006977710650566631954
    81234880673210146739058568557934581403627822703280
    82616570773948327592232845941706525094512325230608
    22918802058777319719839450180888072429661980811197
    77158542502016545090413245809786882778948721859617
    72107838435069186155435662884062257473692284509516
    20849603980134001723930671666823555245252804609722
    53503534226472524250874054075591789781264330331690))

(defun problem13 ()
  (subseq (write-to-string (reduce #'+ *input13*)) 0 10))

(defun problem14 ()
  (loop :with best = 1
        :and best-n = 1
        :for n :from 2 :below 1000000
        :do (let ((k (loop :with k = n
                           :while (> k 1)
                           :count t
                           :do (setf k (if (divp k 2) (/ k 2) (+ (* 3 k) 1))))))
              (if (> k best)
                  (setf best k
                        best-n n)))
        :finally (return best-n)))

(defun problem15 ()
  (binom 40 20))

(defun problem16 ()
  (reduce #'+ (map 'list #'digit (write-to-string (expt 2 1000)))))

(defparameter *input18*
  '((75)
    (95 64)
    (17 47 82)
    (18 35 87 10)
    (20 04 82 47 65)
    (19 01 23 75 03 34)
    (88 02 77 73 07 63 67)
    (99 65 04 28 06 16 70 92)
    (41 41 26 56 83 40 80 70 33)
    (41 48 72 33 47 32 37 16 94 29)
    (53 71 44 65 25 43 91 52 97 51 14)
    (70 11 33 28 77 73 17 78 39 68 17 57)
    (91 71 52 38 17 14 91 43 58 50 27 29 48)
    (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
    (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

(defun problem18 ()
  (loop :with best = (car (last *input18*))
        :for row :in (cdr (reverse *input18*))
        :do (setf best (mapcar #'+ row (mapcar #'max best (cdr best))))
        :finally (return best)))

(defun problem19 ()
  (let* ((normal-year '(31 28 31 30 31 30 31 31 30 31 30 31))
         (leap-year '(31 29 31 30 31 30 31 31 30 31 30 31))
         (century (loop :for year :from 1901 :to 2000
                        :append (if (divp year 4) leap-year normal-year))))
    (loop :for month :in century
          :count (= (rem weekday 7) 5)
          :sum month :into weekday)))

(defun problem20 ()
  (reduce #'+ (map 'list #'digit (write-to-string (fact 100)))))

(defun problem21 ()
  (loop :for n :from 1 :below 10000
        :when (let* ((p (reduce #'+ (proper-divisors n)))
                     (q (reduce #'+ (proper-divisors p))))
                (and (/= p n) (= q n)))
        :sum n))

(defun problem23 ()
  (let* ((abundants (loop :for n :from 1 :to 28123
                          :when (abundantp n) :collect n))
         (sums (make-array 28123 :initial-element nil)))
    (loop :for a :in abundants
          :do
          (loop :for b :in abundants
                :when (< (+ a b) (array-dimension sums 0))
                :do (setf (aref sums (+ a b)) t)))
    (loop :for abundant-sum :across sums
          :for n :from 0
          :unless abundant-sum
          :sum n)))

(defun problem24 ()
  (nth-permutation 999999 (iota 10)))

(defun problem25 ()
  (loop :with n = 0
        :and k = 1
        :for i :from 1
        :do (setf (values n k) (values (+ n k) n))
        :when (>= n (expt 10 999))
        :do (return i)))

(defun problem26 ()
  (maximum (lambda (m n) (< (cycle-length (find-cycle (decimal-repr (/ m))))
                            (cycle-length (find-cycle (decimal-repr (/ n))))))
           (cdr (iota 1000))))

(defun problem27 ()
  (reduce #'*
          (loop :for a :from -999 :to 999
                :append
                (loop :for b :from -1000 :to 1000
                      :when
                      (<= 71 (loop :for n :from 0
                                   :while (primep (+ (sqr n) (* a n) b))
                                   :count t))
                      :append (list a b)))))
