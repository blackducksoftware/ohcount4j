* Sample Program Written in Cobol

PROCEDURE DIVISION.
Begin.
    DISPLAY "Enter lower case character or digit. No data ends.".
    ACCEPT Char.
    PERFORM UNTIL NOT ValidCharacter
        EVALUATE TRUE
           WHEN Vowel DISPLAY "The letter " Char " is a vowel."
           WHEN Consonant DISPLAY "The letter " Char " is a consonant."
           WHEN Digit DISPLAY Char " is a digit."
           WHEN OTHER DISPLAY "problems found"
        END-EVALUATE
    END-PERFORM
    STOP RUN.;
    