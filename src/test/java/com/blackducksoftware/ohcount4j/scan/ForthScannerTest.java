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
import static com.blackducksoftware.ohcount4j.Language.FORTH;

import org.testng.annotations.Test;

public class ForthScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(FORTH, new Line(FORTH, BLANK), "\n");
        assertLine(FORTH, new Line(FORTH, BLANK), "     \n");
        assertLine(FORTH, new Line(FORTH, BLANK), "\t\n");
        assertLine(FORTH, new Line(FORTH, CODE), "function factorial(n)\n");
        assertLine(FORTH, new Line(FORTH, COMMENT), "( Block Comment )\n");
        assertLine(FORTH, new Line(FORTH, COMMENT), "\\ Line comment\n");
        assertLine(FORTH, new Line(FORTH, COMMENT), "\\\n");
        assertLine(FORTH, new Line(FORTH, CODE), ": #? ( d1 -- d2 ) \\ with comment\n");
        assertLine(FORTH, new Line(FORTH, COMMENT), "( )\n");
        assertLine(FORTH, new Line(FORTH, COMMENT), "\\(\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(FORTH, new Line(FORTH, BLANK), "     ");
        assertLine(FORTH, new Line(FORTH, BLANK), "\t");
        assertLine(FORTH, new Line(FORTH, CODE), "function factorial(n)");
        assertLine(FORTH, new Line(FORTH, COMMENT), "( Block Comment )");
        assertLine(FORTH, new Line(FORTH, COMMENT), "\\ Line comment");
        assertLine(FORTH, new Line(FORTH, COMMENT), "\\");
        assertLine(FORTH, new Line(FORTH, CODE), ": #? ( d1 -- d2 ) \\ with comment");
        assertLine(FORTH, new Line(FORTH, COMMENT), "\t( )");
        assertLine(FORTH, new Line(FORTH, COMMENT), " ( )");
        assertLine(FORTH, new Line(FORTH, COMMENT), "\\(");
    }

    @Test
    public void sampleTest() {
        String code = "\\ Sample Forth code\n"
                + "\n"
                + "( This is a comment\n"
                + "  spanning multiple lines )\n"
                + "\n"
                + ": somedefinition ;\n"
                + "\n";

        Line[] expected = {
                new Line(FORTH, COMMENT),
                new Line(FORTH, BLANK),
                new Line(FORTH, COMMENT),
                new Line(FORTH, COMMENT),
                new Line(FORTH, BLANK),
                new Line(FORTH, CODE),
                new Line(FORTH, BLANK)
        };
        assertLines(FORTH, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "(\n\n\n";

        Line[] expected = {
                new Line(FORTH, COMMENT),
                new Line(FORTH, BLANK),
                new Line(FORTH, BLANK)
        };
        assertLines(FORTH, expected, code);
    }

}
