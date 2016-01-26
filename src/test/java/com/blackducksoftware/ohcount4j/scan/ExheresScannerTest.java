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

public class ExheresScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.EXHERES, new Line(Language.EXHERES, BLANK), "\n");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, BLANK), "     \n");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, BLANK), "\t\n");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, CODE), "LICENCES=\"BSD\"\n");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, COMMENT), "# Line comment\n");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, COMMENT), "#\n");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, CODE), "MY_BIN=${MY_BIN:-$PN} # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.EXHERES, new Line(Language.EXHERES, BLANK), "     ");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, BLANK), "\t");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, CODE), "LICENCES=\"BSD\"");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, COMMENT), "# Line comment");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, COMMENT), "#");
        assertLine(Language.EXHERES, new Line(Language.EXHERES, CODE), "require distutils # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "# comment 1\n"
                + "\t\n"
                + "# print the name and contents of the current\n"
                + "# working directory\n"
                + "src_prepare() {\n"
                + "default     # print the directory name\n"
                + "edo sed      # print the directory contents\n"
                + "}";

        Line[] expected = {
                new Line(Language.EXHERES, COMMENT),
                new Line(Language.EXHERES, BLANK),
                new Line(Language.EXHERES, COMMENT),
                new Line(Language.EXHERES, COMMENT),
                new Line(Language.EXHERES, CODE),
                new Line(Language.EXHERES, CODE),
                new Line(Language.EXHERES, CODE),
                new Line(Language.EXHERES, CODE)
        };
        assertLines(Language.EXHERES, expected, code);
    }

}
