package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class ModulaScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.MODULA2, new Line(Language.MODULA2, BLANK), "\n");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, BLANK), "     \n");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, BLANK), "\t\n");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, CODE), "IO.Put(\"Hello World\")\n");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, COMMENT), "(* Block Comment *)\n");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, CODE), "IO.Put(\"Hello World\") (* with comment *)\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.MODULA2, new Line(Language.MODULA2, BLANK), "     ");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, BLANK), "\t\n");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, CODE), "IO.Put(\"Hello World\")");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, COMMENT), "(* Block Comment *)");
        assertLine(Language.MODULA2, new Line(Language.MODULA2, CODE), "IO.Put(\"Hello World\") (* with comment *)");
    }

    @Test
    public void sampleTest() {
        String code = "(* Simple Hello World Program\n"
                + "		\n"
                + "		Written in Modula *)\n"
                + "MODULE HelloWorld EXPORTS Main;\n"
                + "IMPORT IO;\n"
                + "BEGIN\n"
                + "		IO.Put(\"Hello World\") (* Prints Hello World*)\n"
                + "END Main.;\n";

        Line[] expected = {
                new Line(Language.MODULA2, COMMENT),
                new Line(Language.MODULA2, BLANK),
                new Line(Language.MODULA2, COMMENT),
                new Line(Language.MODULA2, CODE),
                new Line(Language.MODULA2, CODE),
                new Line(Language.MODULA2, CODE),
                new Line(Language.MODULA2, CODE),
                new Line(Language.MODULA2, CODE)
        };
        assertLines(Language.MODULA2, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "(*\n\n\n";

        Line[] expected = {
                new Line(Language.MODULA2, COMMENT),
                new Line(Language.MODULA2, BLANK),
                new Line(Language.MODULA2, BLANK)
        };
        assertLines(Language.MODULA2, expected, code);
    }

}
