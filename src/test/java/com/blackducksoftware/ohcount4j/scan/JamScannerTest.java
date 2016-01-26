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
import static com.blackducksoftware.ohcount4j.Language.JAM;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class JamScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(JAM, new Line(JAM, BLANK), "\n");
        assertLine(JAM, new Line(JAM, BLANK), "     \n");
        assertLine(JAM, new Line(JAM, BLANK), "\t\n");
        assertLine(JAM, new Line(JAM, CODE), "str = hello : world ;   # defines 'hello', ':', 'world'\n");
        assertLine(JAM, new Line(JAM, COMMENT), "# Line comment\n");
        assertLine(JAM, new Line(JAM, COMMENT), "#\n");
        assertLine(JAM, new Line(JAM, COMMENT), "\t#\n");
        assertLine(JAM, new Line(JAM, CODE), "    Echo $(1) ;\n");
        assertLine(JAM, new Line(JAM, CODE), "str = hello : world ;   # defines 'hello', ':', 'world' # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(JAM, new Line(JAM, BLANK), "     ");
        assertLine(JAM, new Line(JAM, BLANK), "\t");
        assertLine(JAM, new Line(JAM, CODE), "str = hello : world ;");
        assertLine(JAM, new Line(JAM, COMMENT), "# Line comment");
        assertLine(JAM, new Line(JAM, COMMENT), "#");
        assertLine(JAM, new Line(JAM, CODE), "str = hello : world ; # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "rule Dump\n"
                + "{\n"
                + "Echo $(1) ;\n"
                + "Echo $(2) ;\n"
                + "}\n"
                + "\n"
                + "# defines 'hello', ':', 'world'\n"
                + "     # defines 'hello', ':', 'world'\n"
                + "\t# defines 'hello', ':', 'world'\n"
                + "str = hello : world ; # defines 'hello', ':', 'world'\n"
                + "Dumphello world ;\n"
                + "Dumphello : world ;\n"
                + "Dump$(str) ;\n";

        Line[] expected = {
                new Line(JAM, CODE),
                new Line(JAM, CODE),
                new Line(JAM, CODE),
                new Line(JAM, CODE),
                new Line(JAM, CODE),
                new Line(JAM, BLANK),
                new Line(JAM, COMMENT),
                new Line(JAM, COMMENT),
                new Line(JAM, COMMENT),
                new Line(JAM, CODE),
                new Line(JAM, CODE),
                new Line(JAM, CODE),
                new Line(JAM, CODE),
        };
        assertLines(JAM, expected, code);
    }

}
