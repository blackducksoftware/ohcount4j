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
import static com.blackducksoftware.ohcount4j.Language.MAKE;

import org.testng.annotations.Test;

public class MakeScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(MAKE, new Line(MAKE, BLANK), "\n");
        assertLine(MAKE, new Line(MAKE, BLANK), "     \n");
        assertLine(MAKE, new Line(MAKE, BLANK), "\t\n");
        assertLine(MAKE, new Line(MAKE, CODE), "install: \n");
        assertLine(MAKE, new Line(MAKE, COMMENT), "# Line comment\n");
        assertLine(MAKE, new Line(MAKE, COMMENT), "#\n");
        assertLine(MAKE, new Line(MAKE, CODE), "@for dir in $(SUBDIRS); do # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(MAKE, new Line(MAKE, BLANK), "     ");
        assertLine(MAKE, new Line(MAKE, BLANK), "\t");
        assertLine(MAKE, new Line(MAKE, CODE), "uninstall:");
        assertLine(MAKE, new Line(MAKE, COMMENT), "# Line comment");
        assertLine(MAKE, new Line(MAKE, COMMENT), "#");
        assertLine(MAKE, new Line(MAKE, CODE), "-include config.mk # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "-include config.mk\n"
        		+"#Comment 1\n"
        		+"SUBDIRS	= include lib bin data\n"
        		+"\n"
        		+"ifdef BUILD_API_DOCS\n"
        		+"SUBDIRS += doc\n"
        		+"endif\n"
        		+"\n"
        		+"# Comment 2\n";

        Line[] expected = {
                new Line(MAKE, CODE),
                new Line(MAKE, COMMENT),
                new Line(MAKE, CODE),
                new Line(MAKE, BLANK),
                new Line(MAKE, CODE),
                new Line(MAKE, CODE),
                new Line(MAKE, CODE),
                new Line(MAKE, BLANK),
                new Line(MAKE, COMMENT)
        };
        assertLines(MAKE, expected, code);
    }

}
