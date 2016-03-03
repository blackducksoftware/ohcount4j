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
import static com.blackducksoftware.ohcount4j.Language.DYLAN;

import org.testng.annotations.Test;

public class DylanScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(DYLAN, new Line(DYLAN, BLANK), "\n");
        assertLine(DYLAN, new Line(DYLAN, BLANK), "     \n");
        assertLine(DYLAN, new Line(DYLAN, BLANK), "\t\n");
        assertLine(DYLAN, new Line(DYLAN, CODE), "describe-list(#(1, 2, 3, 4, 5, 6));\n");
        assertLine(DYLAN, new Line(DYLAN, COMMENT), "/* prints \"{a <list>, size: 6}\" */\n");
        assertLine(DYLAN, new Line(DYLAN, COMMENT), "// prints \"{a <list>, size: 6}\"\n");
        assertLine(DYLAN, new Line(DYLAN, COMMENT), "//\n");
        assertLine(DYLAN, new Line(DYLAN, CODE), "describe-list(#(1, 2, 3, 4, 5, 6)); // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(DYLAN, new Line(DYLAN, BLANK), "     ");
        assertLine(DYLAN, new Line(DYLAN, BLANK), "\t");
        assertLine(DYLAN, new Line(DYLAN, CODE), "    format(*standard-output*, \", elements:\");");
        assertLine(DYLAN, new Line(DYLAN, COMMENT), "/* Block Comment */");
        assertLine(DYLAN, new Line(DYLAN, COMMENT), "// prints \"{a <list>, size: 3, elements: 5 7 3}\"");
        assertLine(DYLAN, new Line(DYLAN, COMMENT), "//");
        assertLine(DYLAN, new Line(DYLAN, CODE), "define constant $pi :: <double-float> = 3.1415927d0; // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "define class <point> (<object>)\n"
                + "  slot point-x :: <integer>,\n"
                + "// comment 1\n"
                + "\n"
                + " \n"
                + "\t\n"
                + "    required-init-keyword: x:;\n"
                + "  slot point-y :: <integer>,\n"
                + "    required-init-keyword: y:;\n"
                + "end class <point>\n";

        Line[] expected = {
                new Line(DYLAN, CODE),
                new Line(DYLAN, CODE),
                new Line(DYLAN, COMMENT),
                new Line(DYLAN, BLANK),
                new Line(DYLAN, BLANK),
                new Line(DYLAN, BLANK),
                new Line(DYLAN, CODE),
                new Line(DYLAN, CODE),
                new Line(DYLAN, CODE),
                new Line(DYLAN, CODE)
        };
        assertLines(DYLAN, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(DYLAN, CODE),
                new Line(DYLAN, CODE),
                new Line(DYLAN, BLANK)
        };
        assertLines(DYLAN, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(DYLAN, COMMENT),
                new Line(DYLAN, BLANK),
                new Line(DYLAN, BLANK)
        };
        assertLines(DYLAN, expected, code);
    }
}
