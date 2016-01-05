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

public class FortranFixedScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, BLANK), "\n");
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, BLANK), "     \n");
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, CODE), "       print *, \"Hello World!\"\n");
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C Line comment\n");
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, BLANK), "     ");
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, CODE), "       print *, \"Hello World!\"");
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C Line comment");
        assertLine(Language.FORTRANFIXED, new Line(Language.FORTRANFIXED, COMMENT), "C");
    }

    @Test
    public void sampleTest() {
        String code = "C Simple Hello World Program\n"
                + "C Written in Fortran Fixed Format\n"
                + "       program hello\n"
                + "          print *, \"Hello World!\"\n"
                + "	      end program hello\n";

        Line[] expected = {
                new Line(Language.FORTRANFIXED, COMMENT),
                new Line(Language.FORTRANFIXED, COMMENT),
                new Line(Language.FORTRANFIXED, CODE),
                new Line(Language.FORTRANFIXED, CODE),
                new Line(Language.FORTRANFIXED, CODE)
        };
        assertLines(Language.FORTRANFIXED, expected, code);
    }

}
