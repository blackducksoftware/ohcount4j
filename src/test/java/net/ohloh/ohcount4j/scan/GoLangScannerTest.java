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
public class GoLangScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "     \n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "\t\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "package main;\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\";\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "/* Block Comment */\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "// Line comment\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "//\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\"; // with comment\n");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "func functionDeclaration() {");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "fmt.Println(\"Hello, World!\")");
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
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, CODE)
        };
        assertLines(Language.GOLANG, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "     ");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, BLANK), "\t");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "package main;");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\";");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "/* Block Comment */");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "// Line comment");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, COMMENT), "//");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "import \"fmt\"; // with comment");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "func functionDeclaration() {");
        assertLine(Language.GOLANG, new Line(Language.GOLANG, CODE), "fmt.Println(\"Hello, World!\")");
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, CODE),
                new Line(Language.GOLANG, BLANK)
        };
        assertLines(Language.GOLANG, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(Language.GOLANG, COMMENT),
                new Line(Language.GOLANG, BLANK),
                new Line(Language.GOLANG, BLANK)
        };
        assertLines(Language.GOLANG, expected, code);
    }
}
