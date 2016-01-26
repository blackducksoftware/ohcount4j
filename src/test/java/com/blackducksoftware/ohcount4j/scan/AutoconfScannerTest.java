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
import static com.blackducksoftware.ohcount4j.Language.AUTOCONF;

import org.testng.annotations.Test;

public class AutoconfScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(AUTOCONF, new Line(AUTOCONF, BLANK), "\n");
        assertLine(AUTOCONF, new Line(AUTOCONF, BLANK), "     \n");
        assertLine(AUTOCONF, new Line(AUTOCONF, BLANK), "\t\n");
        assertLine(AUTOCONF, new Line(AUTOCONF, CODE), "echo \"hello\"\n");
        assertLine(AUTOCONF, new Line(AUTOCONF, COMMENT), "dnl Line comment\n");
        assertLine(AUTOCONF, new Line(AUTOCONF, COMMENT), "dnl\n");
        assertLine(AUTOCONF, new Line(AUTOCONF, CODE), "ls # with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(AUTOCONF, new Line(AUTOCONF, BLANK), "     ");
        assertLine(AUTOCONF, new Line(AUTOCONF, BLANK), "\t");
        assertLine(AUTOCONF, new Line(AUTOCONF, CODE), "echo \"hello\"");
        assertLine(AUTOCONF, new Line(AUTOCONF, COMMENT), "dnl Line comment");
        assertLine(AUTOCONF, new Line(AUTOCONF, COMMENT), "dnl");
        assertLine(AUTOCONF, new Line(AUTOCONF, CODE), "ls # with comment");
    }

    @Test
    public void simpleTest() {
        String code = "dnl!/bin/bash\n"
                + "\t\n"
                + "dnl print the name and contents of the current\n"
                + "dnl working directory\n"
                + "dnl comment with dnl\n"
                + "echo \"This is from a shell script:\"\n"
                + "pwd     # print the directory name\n"
                + "ls      # print the directory contents\n";

        Line[] expected = {
                new Line(AUTOCONF, COMMENT),
                new Line(AUTOCONF, BLANK),
                new Line(AUTOCONF, COMMENT),
                new Line(AUTOCONF, COMMENT),
                new Line(AUTOCONF, COMMENT),
                new Line(AUTOCONF, CODE),
                new Line(AUTOCONF, CODE),
                new Line(AUTOCONF, CODE)
        };
        assertLines(AUTOCONF, expected, code);
    }

    @Test
    public void simpleTest2() {
        String code = "ABS_CLIENT_LIBJVM_SO=\"${prefix}/jre/lib/${INSTALL_ARCH_DIR}/client/libjvm.so\"\n" +
                "ABS_SERVER_LIBJVM_SO=\"${prefix}/jre/lib/${INSTALL_ARCH_DIR}/server/libjvm.so\"\n" +
                " dnl comment 1\n" +
                " # not a comment in autoconf\n" +
                "AC_SUBST(ABS_CLIENT_LIBJVM_SO)\n" +
                "AC_SUBST(ABS_SERVER_LIBJVM_SO)\n";
        Line[] expected = {
                new Line(AUTOCONF, CODE),
                new Line(AUTOCONF, CODE),
                new Line(AUTOCONF, COMMENT),
                new Line(AUTOCONF, CODE),
                new Line(AUTOCONF, CODE),
                new Line(AUTOCONF, CODE)
        };
        assertLines(AUTOCONF, expected, code);
    }

}
