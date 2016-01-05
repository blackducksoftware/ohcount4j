/**
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
import static com.blackducksoftware.ohcount4j.Language.AUTOMAKE;

import org.testng.annotations.Test;

/**
 * @author mpujari
 *
 */
public class AutomakeScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(AUTOMAKE, new Line(AUTOMAKE, BLANK), "\n");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, BLANK), "     \n");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, BLANK), "\t\n");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, CODE), "echo \"hello\"\n");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, COMMENT), "# Line comment\n");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, COMMENT), "#\n");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, CODE), "ls # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(AUTOMAKE, new Line(AUTOMAKE, BLANK), "     ");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, BLANK), "\t");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, CODE), "echo \"hello\"");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, COMMENT), "# Line comment");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, COMMENT), "#");
        assertLine(AUTOMAKE, new Line(AUTOMAKE, CODE), "ls # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "#!/bin/bash\n"
                + "\t\n"
                + "# print the name and contents of the current\n"
                + "# working directory\n"
                + "dnl its not a comment\n"
                + "echo \"This is from a shell script:\"\n"
                + "pwd     # print the directory name\n"
                + "ls      # print the directory contents\n";

        Line[] expected = {
                new Line(AUTOMAKE, COMMENT),
                new Line(AUTOMAKE, BLANK),
                new Line(AUTOMAKE, COMMENT),
                new Line(AUTOMAKE, COMMENT),
                new Line(AUTOMAKE, CODE),
                new Line(AUTOMAKE, CODE),
                new Line(AUTOMAKE, CODE),
                new Line(AUTOMAKE, CODE)
        };
        assertLines(AUTOMAKE, expected, code);
    }

    @Test
    public void simpleTest2() {
        String code = "ABS_CLIENT_LIBJVM_SO=\"${prefix}/jre/lib/${INSTALL_ARCH_DIR}/client/libjvm.so\"\n" +
                "ABS_SERVER_LIBJVM_SO=\"${prefix}/jre/lib/${INSTALL_ARCH_DIR}/server/libjvm.so\"\n" +
                " # comment 1\n" +
                "AC_SUBST(ABS_CLIENT_LIBJVM_SO)\n" +
                "AC_SUBST(ABS_SERVER_LIBJVM_SO)\n";
        Line[] expected = {
                new Line(AUTOMAKE, CODE),
                new Line(AUTOMAKE, CODE),
                new Line(AUTOMAKE, COMMENT),
                new Line(AUTOMAKE, CODE),
                new Line(AUTOMAKE, CODE)
        };
        assertLines(AUTOMAKE, expected, code);
    }

}
