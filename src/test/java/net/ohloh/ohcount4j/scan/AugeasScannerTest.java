/**
 * Copyright (C) 2015 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import net.ohloh.ohcount4j.Language;

import org.testng.annotations.Test;

/**
 * @author gandhip
 *
 */
public class AugeasScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "     \n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "\t\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "module Modules =\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "autoload xfm\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, COMMENT), "(* Block Comment *)\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let word = /[^#, \\n\\t\\/]+/ \n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let record = [ key word . (Util.del_ws_tab . sto_line)? . Util.eol ] \n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let filter = incl \"/etc/modules\"\n");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "test lns get \"key = value\" = ?\n");
    }

    @Test
    public void helloWorld() {
        String code = "(* Hello World\n" // Multi-line comment
                + " * with multi-line comment *)\n"
                + "\n"
                + "(* Hello \n" // Nested Comments
                + "(* World \n"
                + " * with *)\n"
                + "nested comment *)\n"
                + "\n"
                + "package main\n"
                + "\n"
                + "import \"fmt\"\n"
                + "\n"
                + "\tfunc main() { \n"
                + "(* single line comment *)\n" // single line comment
                + "\t\tvar x float64 = 20.0\n"
                + "\t\tfmt.Println(\"Hello, World!\")\n"
                + "\t}\n";
        Line[] expected = {
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, BLANK),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, COMMENT),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, CODE)
        };
        assertLines(Language.AUGEAS, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "     ");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, BLANK), "\t");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "module Modules =");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "autoload xfm");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, COMMENT), "(* Block Comment *)");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let word = /[^#, \\n\\t\\/]+/ ");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let record = [ key word . (Util.del_ws_tab . sto_line)? . Util.eol ] ");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "let filter = incl \"/etc/modules\"");
        assertLine(Language.AUGEAS, new Line(Language.AUGEAS, CODE), "test lns get \"key = value\" = ?");
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        String code = "\"\nA\n\n";

        Line[] expected = {
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, CODE),
                new Line(Language.AUGEAS, BLANK)
        };
        assertLines(Language.AUGEAS, expected, code);
    }

}
