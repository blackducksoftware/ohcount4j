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

public class ShellScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.SHELL, new Line(Language.SHELL, BLANK), "\n");
        assertLine(Language.SHELL, new Line(Language.SHELL, BLANK), "     \n");
        assertLine(Language.SHELL, new Line(Language.SHELL, BLANK), "\t\n");
        assertLine(Language.SHELL, new Line(Language.SHELL, CODE), "echo \"hello\"\n");
        assertLine(Language.SHELL, new Line(Language.SHELL, COMMENT), "# Line comment\n");
        assertLine(Language.SHELL, new Line(Language.SHELL, COMMENT), "#\n");
        assertLine(Language.SHELL, new Line(Language.SHELL, CODE), "ls # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.SHELL, new Line(Language.SHELL, BLANK), "     ");
        assertLine(Language.SHELL, new Line(Language.SHELL, BLANK), "\t");
        assertLine(Language.SHELL, new Line(Language.SHELL, CODE), "echo \"hello\"");
        assertLine(Language.SHELL, new Line(Language.SHELL, COMMENT), "# Line comment");
        assertLine(Language.SHELL, new Line(Language.SHELL, COMMENT), "#");
        assertLine(Language.SHELL, new Line(Language.SHELL, CODE), "ls # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "#!/bin/bash\n"
                + "\t\n"
                + "# print the name and contents of the current\n"
                + "# working directory\n"
                + "echo \"This is from a shell script:\"\n"
                + "pwd     # print the directory name\n"
                + "ls      # print the directory contents\n";

        Line[] expected = {
                new Line(Language.SHELL, COMMENT),
                new Line(Language.SHELL, BLANK),
                new Line(Language.SHELL, COMMENT),
                new Line(Language.SHELL, COMMENT),
                new Line(Language.SHELL, CODE),
                new Line(Language.SHELL, CODE),
                new Line(Language.SHELL, CODE)
        };
        assertLines(Language.SHELL, expected, code);
    }

}
