/*
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
import static net.ohloh.ohcount4j.Language.SCALA;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class ScalaScannerTeset extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(SCALA, new Line(SCALA, BLANK), "\n");
        assertLine(SCALA, new Line(SCALA, BLANK), "     \n");
        assertLine(SCALA, new Line(SCALA, BLANK), "\t\n");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/* Block Comment */\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/** Block Comment **/\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/*** Block Comment ***/\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "// Line comment\n");
        assertLine(SCALA, new Line(SCALA, COMMENT), "//\n");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._ // with comment\n");
        assertLine(SCALA, new Line(SCALA, CODE), "val counts = holmes.split(\"\\s+\").groupBy(x=>x)");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(SCALA, new Line(SCALA, BLANK), "     ");
        assertLine(SCALA, new Line(SCALA, BLANK), "\t");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/* Block Comment */");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/** Block Comment **/");
        assertLine(SCALA, new Line(SCALA, COMMENT), "/*** Block Comment ***/");
        assertLine(SCALA, new Line(SCALA, COMMENT), "// Line comment");
        assertLine(SCALA, new Line(SCALA, COMMENT), "//");
        assertLine(SCALA, new Line(SCALA, CODE), "import java.io._ // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "import java.io._\n"
                + "\n"
                + "object HelloWorld {\n"
                + "    def main(args: Array[String]): Unit = {\n"
                + "    println(\"Hello, world!\")\n"
                + "/** comment yet again **/\n"
                + "/*** comment yet again ***/\n"
                + "}\n}";

        Line[] expected = {
                new Line(SCALA, COMMENT),
                new Line(SCALA, COMMENT),
                new Line(SCALA, BLANK),
                new Line(SCALA, CODE),
                new Line(SCALA, BLANK),
                new Line(SCALA, CODE),
                new Line(SCALA, CODE),
                new Line(SCALA, CODE),
                new Line(SCALA, COMMENT),
                new Line(SCALA, COMMENT),
                new Line(SCALA, CODE),
                new Line(SCALA, CODE)
        };
        assertLines(SCALA, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(SCALA, CODE),
                new Line(SCALA, CODE),
                new Line(SCALA, BLANK)
        };
        assertLines(SCALA, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(SCALA, COMMENT),
                new Line(SCALA, BLANK),
                new Line(SCALA, BLANK)
        };
        assertLines(SCALA, expected, code);
    }

}
