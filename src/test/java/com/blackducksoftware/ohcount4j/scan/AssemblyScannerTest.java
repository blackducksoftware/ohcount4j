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

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class AssemblyScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK), "\n");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK), "     \n");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK), "\t\n");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE), "mov(ax, bx);\n");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "/* Block Comment */\n");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "# Line comment\n");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "#\n");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE), "mov ax, bx    ;we move bx into ax\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK), "     ");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, BLANK), "\t");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE), "mov(ax, bx);");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "/* Block Comment */");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "# Line comment");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, COMMENT), "#");
        assertLine(Language.ASSEMBLY, new Line(Language.ASSEMBLY, CODE), "mov ax, bx    ;we move bx into ax");
    }

    @Test
    public void sampleTest() {
        String code = "/* Simple Assembly Program\n"
                + "\t\n"
                + "*/\n"
                + "# load $t3\n"
                + "lb	$t3, ($t0)\n"
                + "; store contents of $t3\n"
                + "sb	$t3, ($t1)\n"
                + "! decrement $t2\n"
                + "sub	$t2, $t2, 1\n"
                + "add	$t0, $t0, 1	; increment $t0\n"
                + "add	$t1, $t1, 1	; increment $t1\n"
                + "\n";

        Line[] expected = {
                new Line(Language.ASSEMBLY, COMMENT),
                new Line(Language.ASSEMBLY, BLANK),
                new Line(Language.ASSEMBLY, COMMENT),
                new Line(Language.ASSEMBLY, COMMENT),
                new Line(Language.ASSEMBLY, CODE),
                new Line(Language.ASSEMBLY, COMMENT),
                new Line(Language.ASSEMBLY, CODE),
                new Line(Language.ASSEMBLY, COMMENT),
                new Line(Language.ASSEMBLY, CODE),
                new Line(Language.ASSEMBLY, CODE),
                new Line(Language.ASSEMBLY, CODE),
                new Line(Language.ASSEMBLY, BLANK)
        };
        assertLines(Language.ASSEMBLY, expected, code);
    }

}
