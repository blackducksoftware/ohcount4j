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

public class AdaScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.ADA, new Line(Language.ADA, BLANK), "\n");
        assertLine(Language.ADA, new Line(Language.ADA, BLANK), "     \n");
        assertLine(Language.ADA, new Line(Language.ADA, BLANK), "\t\n");
        assertLine(Language.ADA, new Line(Language.ADA, CODE), "Ada.Text_IO.Put_Line (\"Hello World\");\n");
        assertLine(Language.ADA, new Line(Language.ADA, COMMENT), "-- Line comment\n");
        assertLine(Language.ADA, new Line(Language.ADA, COMMENT), "--\n");
        assertLine(Language.ADA, new Line(Language.ADA, CODE), "Ada.Text_IO.Put_Line (\"Hello World\"); -- with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.ADA, new Line(Language.ADA, BLANK), "     ");
        assertLine(Language.ADA, new Line(Language.ADA, BLANK), "\t");
        assertLine(Language.ADA, new Line(Language.ADA, CODE), "Ada.Text_IO.Put_Line (\"Hello World\");");
        assertLine(Language.ADA, new Line(Language.ADA, COMMENT), "-- Line comment");
        assertLine(Language.ADA, new Line(Language.ADA, COMMENT), "--");
        assertLine(Language.ADA, new Line(Language.ADA, CODE), "Ada.Text_IO.Put_Line (\"Hello World\"); -- with comment");
    }

    @Test
    public void sampleTest() {
        String code = "-- Sample Test Program\n"
                + "--\n"
                + "\t\n"
                + "for i in 1 .. 10 loop\n"
                + "		Ada.Text_IO.Put (\"Iteration: \");\n"
                + "		Ada.Text_IO.Put (i); -- Print the current iteration\n"
                + "		Ada.Text_IO.Put_Line;\n"
                + "end loop;\n";

        Line[] expected = {
                new Line(Language.ADA, COMMENT),
                new Line(Language.ADA, COMMENT),
                new Line(Language.ADA, BLANK),
                new Line(Language.ADA, CODE),
                new Line(Language.ADA, CODE),
                new Line(Language.ADA, CODE),
                new Line(Language.ADA, CODE),
                new Line(Language.ADA, CODE)
        };

        assertLines(Language.ADA, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "\"\nA\n\n";

        Line[] expected = {
                new Line(Language.ADA, CODE),
                new Line(Language.ADA, CODE),
                new Line(Language.ADA, BLANK)
        };
        assertLines(Language.ADA, expected, code);
    }

}
