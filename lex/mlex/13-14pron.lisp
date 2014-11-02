(in-package "MORPHIX")

;; 1400 = Personalpronomen
;; 1300 = Reflexiv (+Pers.) - Pronomen

(l-f "ich" '(1400000 (FLEXION ((1 ((SG (NOM))))))))
(l-f "mich" '(1300000 (FLEXION ((1 ((SG (AKK)))))))) ;; neu
(l-f "mir" '(1300000 (FLEXION ((1 ((SG (DAT)))))))) ;; neu
(l-f "meiner" '("meiner" 1410000 (FLEXION ((1 ((SG (GEN)))))))) ;; neu

(l-f "du" '(1400000 (FLEXION ((2 ((SG (NOM))))))))
(l-f "dich" '(1300000 (FLEXION ((2 ((SG (AKK))))))))
(l-f "dir" '(1300000 (FLEXION ((2 ((SG (DAT))))))))
(l-f "deiner" '("deiner" 1410000 (FLEXION ((2 ((SG (GEN)))))))) ;; neu

(l-f "er" '(1400000 (FLEXION ((3 ((MAS ((SG (NOM))))))))))
(l-f "ihn" '("ihn" 1400000 (FLEXION ((3 ((MAS ((SG (AKK))))))))))

(l-f "es" '(1400000 (FLEXION ((3 ((NTR ((SG (NOM AKK))))))))))

(l-f "ihm" '("ihm" 1400000 (FLEXION ((3 ((MAS ((SG (DAT))))
                                         (NTR ((SG (DAT)))))))))) ;;; neu

(l-f "seiner" '("seiner" 1410000 
               (FLEXION ((3 ((MAS ((SG (GEN)))) (NTR ((SG (GEN)))))))))) ;; neu



(l-f "sie" '(1400000
             (FLEXION ((2a ((SG (NOM AKK))
                            (PL (NOM AKK))))
                       (3 ((FEM ((SG (NOM AKK))
                                 (PL (NOM AKK))))
                           (MAS ((PL (NOM AKK))))
                           (NTR ((PL (NOM AKK))))))))))

(l-f "ihr" '(1410000 (FLEXION ((2 ((PL (NOM))))))))
(l-f "ihrer" '("ihrer" 1410000
               (FLEXION ((2a ((SG (GEN)) (PL (GEN))))
                         (3 ((FEM ((SG (GEN)) (PL (GEN))))
                             (MAS ((PL (GEN))))
                             (NTR ((PL (GEN)))))))))) ;;; neu


(l-f "wir" '(1400000 (FLEXION ((1 ((PL (NOM))))))))
(l-f "uns" '(1400000 (FLEXION ((1 ((PL (DAT AKK))))))))
(l-f "unser" '("unser" 1410000 (FLEXION ((1 ((PL (GEN)))))))) ;; neu


(l-f "euch" '(1410000 (FLEXION ((2 ((PL (DAT AKK))))))))
(l-f "euer" '(1410000 (FLEXION ((2 ((PL (GEN))))))))


(l-f "ihnen" '("ihnen" 1400000 (FLEXION ((2a ((SG (DAT)) (PL (DAT))))
                                         (3 ((MAS ((PL (DAT))))
                                            (FEM ((PL (DAT))))
                                            (NTR ((PL (DAT)))))))))) ;;; neu


(l-f "sich" '(1300000 (FLEXION ((3 ((SG (DAT AKK))
                                    (PL (DAT AKK))))
                                (2a ((SG (DAT AKK))
                                     (PL (DAT AKK))))))))

