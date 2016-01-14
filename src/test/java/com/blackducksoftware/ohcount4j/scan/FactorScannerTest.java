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
import static com.blackducksoftware.ohcount4j.Language.FACTOR;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class FactorScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(FACTOR, new Line(FACTOR, BLANK), "\n");
        assertLine(FACTOR, new Line(FACTOR, BLANK), "     \n");
        assertLine(FACTOR, new Line(FACTOR, BLANK), "\t\n");
        assertLine(FACTOR, new Line(FACTOR, CODE), "echo \"hello\"\n");
        assertLine(FACTOR, new Line(FACTOR, CODE), "BEGIN { ");
        assertLine(FACTOR, new Line(FACTOR, COMMENT), "! Line comment\n");
        assertLine(FACTOR, new Line(FACTOR, COMMENT), "!\n");
        assertLine(FACTOR, new Line(FACTOR, COMMENT), "!   ");
        assertLine(FACTOR, new Line(FACTOR, CODE), "USING: kernel sequences ; ! this is not a comment\n");
    }

    @Test
    public void eofHandling() {
        assertLine(FACTOR, new Line(FACTOR, BLANK), "     ");
        assertLine(FACTOR, new Line(FACTOR, BLANK), "\t");
        assertLine(FACTOR, new Line(FACTOR, CODE), "IN: palindrome");
        assertLine(FACTOR, new Line(FACTOR, COMMENT), "! Line comment");
        assertLine(FACTOR, new Line(FACTOR, COMMENT), "!");
        assertLine(FACTOR, new Line(FACTOR, CODE), "USING: kernel sequences ; ! with comment");
    }

    @Test
    public void simpleTest() {
        String code = "! Copyright (C) 2012 Your name.\n"
                + "! See http://factorcode.org/license.txt for BSD license.\n"
                + "USING: ;\n"
                + "IN: palindrome\n"
                + "\n"
                + ": palindrome? ( string -- ? ) dup reverse = ;\n";

        Line[] expected = {
                new Line(FACTOR, COMMENT),
                new Line(FACTOR, COMMENT),
                new Line(FACTOR, CODE),
                new Line(FACTOR, CODE),
                new Line(FACTOR, BLANK),
                new Line(FACTOR, CODE),
        };
        assertLines(FACTOR, expected, code);
    }

}
