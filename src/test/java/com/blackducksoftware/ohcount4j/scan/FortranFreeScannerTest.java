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

public class FortranFreeScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK), "\n");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK), "     \n");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK), "\t\n");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, CODE), "Print *, \"Hello World!\"\n");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "! Line comment\n");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "!\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK), "     ");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, BLANK), "\t");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, CODE), "Print *, \"Hello World!\"");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "! Line comment");
        assertLine(Language.FORTRANFREE, new Line(Language.FORTRANFREE, COMMENT), "!");
    }

    @Test
    public void sampleTest() {
        String code = "! Simple Hello World Program\n"
                + "! Written in Fortran Free Format\n"
                + "Program Hello\n"
                + "   Print *, \"Hello World!\"\n"
                + "End Program Hello\n";

        Line[] expected = {
                new Line(Language.FORTRANFREE, COMMENT),
                new Line(Language.FORTRANFREE, COMMENT),
                new Line(Language.FORTRANFREE, CODE),
                new Line(Language.FORTRANFREE, CODE),
                new Line(Language.FORTRANFREE, CODE)
        };
        assertLines(Language.FORTRANFREE, expected, code);
    }

}
