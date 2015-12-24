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

import java.io.File;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.Test;

/**
 * @author gandhip
 *
 */
public class ChaiScriptScannerTest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "     \n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "\t\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "load_module(\"stl_extra\")\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "/* Block Comment */\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "// Line comment\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "//\n");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "var v = [] // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "     ");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, BLANK), "\t");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "load_module(\"stl_extra\")");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "/* Block Comment */");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "// Line comment");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, COMMENT), "//");
        assertLine(Language.CHAISCRIPT, new Line(Language.CHAISCRIPT, CODE), "var v = [] // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "var submenu=1;\n"
                + "\n"
                + "def print() {\n"
                + "  print(\"Hello world!\")\n"
                + "}";

        Line[] expected = {
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE)
        };
        assertLines(Language.CHAISCRIPT, expected, code);
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("chaiscript.chai")));
        Line[] expected = {
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, BLANK),
                new Line(Language.CHAISCRIPT, COMMENT),
                new Line(Language.CHAISCRIPT, CODE),
                new Line(Language.CHAISCRIPT, CODE)
        };
        assertLines(Language.CHAISCRIPT, expected, sourceFile);
    }

}
