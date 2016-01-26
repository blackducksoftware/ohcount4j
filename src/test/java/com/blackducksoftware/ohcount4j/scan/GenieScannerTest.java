/*
 * Copyright 2016 Black Duck Software, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

/**
 * @author mpujari
 *
 */
public class GenieScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.GENIE, new Line(Language.GENIE, BLANK), "\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, BLANK), "     \n");
        assertLine(Language.GENIE, new Line(Language.GENIE, BLANK), "\t\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "class BasicSample : Object\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "stdout.printf (\"Hello World\\n\")\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, COMMENT), "/* Block Comment */\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, COMMENT), "// Line comment\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, COMMENT), "//\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "init // with comment\n");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "def run ()");
    }

    @Test
    public void helloWorld() {
        String code = "/* class derived from GObject */\n"
                + "class BasicSample : Object \n"
                + "\n"
                + "        /* public instance method */\n"
                + "        def run () \n"
                + "                stdout.printf (\"Hello World\\n\")\n"
                + "\n"
                + "/* application entry point */\n"
                + "init\n"
                + "        // instantiate this class, assigning the instance to\n"
                + "        // a type-inferred variable\n"
                + "        var sample = new BasicSample ()\n"
                + "        // call the run method\n"
                + "        sample.run ()\n"
                + "        // return from this main method\n";
        Line[] expected = {
                new Line(Language.GENIE, COMMENT),
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, BLANK),
                new Line(Language.GENIE, COMMENT),
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, BLANK),
                new Line(Language.GENIE, COMMENT),
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, COMMENT),
                new Line(Language.GENIE, COMMENT),
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, COMMENT),
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, COMMENT),
        };
        assertLines(Language.GENIE, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.GENIE, new Line(Language.GENIE, BLANK), "     ");
        assertLine(Language.GENIE, new Line(Language.GENIE, BLANK), "\t");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "package main;");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "import \"fmt\";");
        assertLine(Language.GENIE, new Line(Language.GENIE, COMMENT), "/* Block Comment */");
        assertLine(Language.GENIE, new Line(Language.GENIE, COMMENT), "// Line comment");
        assertLine(Language.GENIE, new Line(Language.GENIE, COMMENT), "//");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "import \"fmt\"; // with comment");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "func functionDeclaration() {");
        assertLine(Language.GENIE, new Line(Language.GENIE, CODE), "fmt.Println(\"Hello, World!\")");
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, CODE),
                new Line(Language.GENIE, BLANK)
        };
        assertLines(Language.GENIE, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(Language.GENIE, COMMENT),
                new Line(Language.GENIE, BLANK),
                new Line(Language.GENIE, BLANK)
        };
        assertLines(Language.GENIE, expected, code);
    }
}
