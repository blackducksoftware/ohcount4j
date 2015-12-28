package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class RexxScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.REXX, new Line(Language.REXX, BLANK), "\n");
        assertLine(Language.REXX, new Line(Language.REXX, BLANK), "     \n");
        assertLine(Language.REXX, new Line(Language.REXX, BLANK), "\t\n");
        assertLine(Language.REXX, new Line(Language.REXX, CODE), "add_word: procedure expose count. word_list\n");
        assertLine(Language.REXX, new Line(Language.REXX, COMMENT), "/* Block Comment */\n");
        assertLine(Language.REXX, new Line(Language.REXX, CODE), "add_word: procedure expose count. word_list /* with comment */\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.REXX, new Line(Language.REXX, BLANK), "     ");
        assertLine(Language.REXX, new Line(Language.REXX, BLANK), "\t");
        assertLine(Language.REXX, new Line(Language.REXX, CODE), "add_word: procedure expose count. word_list");
        assertLine(Language.REXX, new Line(Language.REXX, COMMENT), "/* Block Comment */");
        assertLine(Language.REXX, new Line(Language.REXX, CODE), "add_word: procedure expose count. word_list /* with comment */");
    }

    @Test
    public void sampleTest() {
        String code = "/*\n"
                + "Sample Rexx Program\n"
                + "		/*\n"
                + "		Can have nested comments\n"
                + "\n"
                + "		*/\n"
                + "*/\n"
                + "\t\n"
                + "add_word: procedure expose dictionary.\n"
                + "		parse arg w .\n"
                + "		dictionary.w = dictionary.w + 1\n"
                + "		if dictionary.w = 1 /* assume dictionary. = 0 */\n"
                + "			then do\n"
                + "				n = dictionary.0+1\n"
                + "				dictionary.n = w\n"
                + "				dictionary.0 = n\n"
                + "			end\n"
                + "		return\n";

        Line[] expected = {
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, BLANK),
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, BLANK),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE),
                new Line(Language.REXX, CODE)
        };
        assertLines(Language.REXX, expected, code);
    }

    @Test
    public void unterminatedNestedCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n/*\n*/\n\n\n";

        Line[] expected = {
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, COMMENT),
                new Line(Language.REXX, BLANK),
                new Line(Language.REXX, BLANK)
        };
        assertLines(Language.REXX, expected, code);
    }
}
