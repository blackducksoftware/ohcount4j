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
public class GoLangScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, BLANK), "\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, BLANK), "     \n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, BLANK), "\t\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "package main;\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "import \"fmt\";\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, COMMENT), "/* Block Comment */\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, COMMENT), "// Line comment\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, COMMENT), "//\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "import \"fmt\"; // with comment\n");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "func functionDeclaration() {");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "fmt.Println(\"Hello, World!\")");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "package main\n"
                + "\n"
                + "import \"fmt\"\n"
                + "\n"
                + "\tfunc main() { \n"
                + "// single line comment\n"
                + "\t\tvar x float64 = 20.0\n"
                + "\t\tfmt.Println(\"Hello, World!\")\n"
                + "\t}\n";
        Line[] expected = {
                new Line(Language.GO_LANG, COMMENT),
                new Line(Language.GO_LANG, COMMENT),
                new Line(Language.GO_LANG, BLANK),
                new Line(Language.GO_LANG, CODE),
                new Line(Language.GO_LANG, BLANK),
                new Line(Language.GO_LANG, CODE),
                new Line(Language.GO_LANG, BLANK),
                new Line(Language.GO_LANG, CODE),
                new Line(Language.GO_LANG, COMMENT),
                new Line(Language.GO_LANG, CODE),
                new Line(Language.GO_LANG, CODE),
                new Line(Language.GO_LANG, CODE)
        };
        assertLines(Language.GO_LANG, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, BLANK), "     ");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, BLANK), "\t");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "package main;");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "import \"fmt\";");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, COMMENT), "/* Block Comment */");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, COMMENT), "// Line comment");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, COMMENT), "//");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "import \"fmt\"; // with comment");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "func functionDeclaration() {");
        assertLine(Language.GO_LANG, new Line(Language.GO_LANG, CODE), "fmt.Println(\"Hello, World!\")");
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(Language.GO_LANG, CODE),
                new Line(Language.GO_LANG, CODE),
                new Line(Language.GO_LANG, BLANK)
        };
        assertLines(Language.GO_LANG, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(Language.GO_LANG, COMMENT),
                new Line(Language.GO_LANG, BLANK),
                new Line(Language.GO_LANG, BLANK)
        };
        assertLines(Language.GO_LANG, expected, code);
    }
}
