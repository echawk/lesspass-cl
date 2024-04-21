(in-package #:lesspass/test)

(5am:def-suite :lesspass)

(5am:def-suite* :lesspass/test :in :lesspass)

(5am:test generate-password
          (let ((mpw "password"))
            (5am:is (string= "WHLpUL)e00[iHR+w"
                             (lesspass:generate-password
                              (make-instance
                               'lesspass:password-profile
                               :site "example.org"
                               :login "contact@example.org"
                               :rules
                               '(lesspass:lowercase
                                 lesspass:uppercase
                                 lesspass:digits
                                 lesspass:symbols)
                               :length 16
                               :counter 1)
                              mpw)))
            (5am:is (string= "WHLpUL)e00[iHR+w"
                             (lesspass:generate-password
                              (make-instance
                               'lesspass:password-profile
                               :site "example.org"
                               :login "contact@example.org"
                               :rules
                               '(lesspass:digits
                                 lesspass:uppercase
                                 lesspass:symbols
                                 lesspass:lowercase)
                               :length 16
                               :counter 1)
                              mpw)))
            (5am:is (string=  "MBAsB7b1Prt8Sl"
                              (lesspass:generate-password
                               (make-instance
                                'lesspass:password-profile
                                :site "example.org"
                                :login "contact@example.org"
                                :rules
                                '(lesspass:lowercase
                                  lesspass:uppercase
                                  lesspass:digits)
                                :length 14
                                :counter 2)
                               mpw)))

            (5am:is (string= "8742368585200667"
                             (lesspass:generate-password
                              (make-instance
                               'lesspass:password-profile
                               :site "example.org"
                               :login "contact@example.org"
                               :rules
                               '(lesspass:digits)
                               :length 16
                               :counter 1)
                              mpw)))

            (5am:is (string= "s>{F}RwkN/-fmM.X"
                             (lesspass:generate-password
                              (make-instance
                               'lesspass:password-profile
                               :site "example.org"
                               :login "contact@example.org"
                               :rules
                               '(lesspass:lowercase
                                 lesspass:uppercase
                                 lesspass:symbols)
                               :length 16
                               :counter 1)
                              mpw)))))


(5am:test generate-password-nrt-328
          (5am:is (string= "XFt0F*,r619:+}[."
                           (lesspass:generate-password
                            (make-instance
                             'lesspass:password-profile
                             :site "site"
                             :login "login"
                             :rules
                             '(lesspass:lowercase
                               lesspass:uppercase
                               lesspass:symbols
                               lesspass:digits)
                             :length 16
                             :counter 10)
                            "test"))))

;; FIXME: get this test to pass!
(5am:test generate-password-unicode
          (5am:is (string= "BH$>U5Lj7v9A1wB/"
                           (lesspass:generate-password
                            (make-instance
                             'lesspass:password-profile
                             :site "♥ LessPass ♥"
                             :login "test@example.org"
                             :rules
                             '(lesspass:lowercase
                               lesspass:uppercase
                               lesspass:symbols
                               lesspass:digits)
                             :length 16
                             :counter 1)
                            "password"))))

