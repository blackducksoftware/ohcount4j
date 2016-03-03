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
import static com.blackducksoftware.ohcount4j.Language.DCL;

import org.testng.annotations.Test;

public class DclMakeScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(DCL, new Line(DCL, BLANK), "\n");
        assertLine(DCL, new Line(DCL, BLANK), "     \n");
        assertLine(DCL, new Line(DCL, BLANK), "\t\n");
        assertLine(DCL, new Line(DCL, CODE), "Variable'i '= \"blue\"\n");
        assertLine(DCL, new Line(DCL, COMMENT), "! -*- coding: UTF-8 -*-\n");
        assertLine(DCL, new Line(DCL, COMMENT), "!\n");
        assertLine(DCL, new Line(DCL, COMMENT), "\t!\n");
        assertLine(DCL, new Line(DCL, CODE), "Color = variable'j '\n");
        assertLine(DCL, new Line(DCL, CODE), "Regenbogen'farbe '= \"yellow\" ! with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(DCL, new Line(DCL, BLANK), "     ");
        assertLine(DCL, new Line(DCL, BLANK), "\t");
        assertLine(DCL, new Line(DCL, CODE), "Color = variable'i '");
        assertLine(DCL, new Line(DCL, COMMENT), "! Line comment");
        assertLine(DCL, new Line(DCL, COMMENT), "!");
        assertLine(DCL, new Line(DCL, CODE), "Color = variable'i ' ! with comment");
    }

    @Test
    public void simpleTest() {

        String code = "I = 1\n"
                + "Variable'i '= \"blue\"\n"
                + "I = 2 \n"
                + "! comment 1\n"
                + "\t! comment 1\n"
                + " \n"
                + "\t\n"
                + "\n"
                + "Variable'i '= \"green\"\n"
                + "J = 1\n"
                + "Color = variable'j '\n"
                + "Regenbogen'farbe '= \"red\"\n"
                + "Color = variable'i '\n"
                + "Regenbogen'farbe '= \"yellow\"\n"
                + "PRINT/COPIES=2 SPRING.SUM,FALL.SUM !some comment\n"
                + "! comment 2";

        Line[] expected = {
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, COMMENT),
                new Line(DCL, COMMENT),
                new Line(DCL, BLANK),
                new Line(DCL, BLANK),
                new Line(DCL, BLANK),
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, CODE),
                new Line(DCL, COMMENT)
        };
        assertLines(DCL, expected, code);
    }

}
