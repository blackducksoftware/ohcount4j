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

public class JavaScriptScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK), "\n");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK), "     \n");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK), "\t\n");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE), "function() {};\n");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, COMMENT), "/* comment */\n");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE), "function() {}; /* with comment */\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK), "     ");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK), "\t");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE), "function() {};");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, COMMENT), "/* comment */");
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE), "function() {}; /* with comment */");
    }

    @Test
    public void escapedCharsInStrings() {
        // A literal newline character embedded within a one-line string should not be
        // incorrectly counted as two lines of code
        assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE),
                "var str = \"a newline literal \\n in a string\";");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World */\n"
                + "\n"
                + "$(document).ready(function() {\n"
                + "\talert(\"Hello, world!\\n\");\n"
                + "});\n";

        Line[] expected = {
                new Line(Language.JAVASCRIPT, COMMENT),
                new Line(Language.JAVASCRIPT, BLANK),
                new Line(Language.JAVASCRIPT, CODE),
                new Line(Language.JAVASCRIPT, CODE),
                new Line(Language.JAVASCRIPT, CODE)
        };
        assertLines(Language.JAVASCRIPT, expected, code);
    }
}
