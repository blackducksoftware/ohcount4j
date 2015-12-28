package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class TclScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "\n");
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "     \n");
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "\t\n");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index)\n");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "# Line comment\n");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "#\n");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index) # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "     ");
        assertLine(Language.TCL, new Line(Language.TCL, BLANK), "\t");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index)");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "# Line comment");
        assertLine(Language.TCL, new Line(Language.TCL, COMMENT), "#");
        assertLine(Language.TCL, new Line(Language.TCL, CODE), "puts $var($index) # with comment");
    }

    @Test
    public void sampleTest() {
        String code = "@x = (1, 2, 3);      # Create a 3 element list/array\n"
                + "\t\n"
                + "foreach $i (@x) {\n"
                + "    print $i++ . \"-\";\n"
                + "}\n"
                + "# At this point, array contains 2, 3, 4!\n";

        Line[] expected = {
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, BLANK),
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, CODE),
                new Line(Language.TCL, COMMENT)
        };
        assertLines(Language.TCL, expected, code);
    }

}
