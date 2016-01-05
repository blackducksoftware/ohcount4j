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

public class FSharpScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK), "\n");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK), "     \n");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK), "\t\n");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE), "let bound = int (System.Math.Sqrt(float n))\n");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "(* Block Comment *)\n");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "// Line comment\n");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "//\n");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE), "let x = 3 + (4 * 5) // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK), "     ");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, BLANK), "\t");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE), "let bound = int (System.Math.Sqrt(float n))");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "(* Block Comment *)");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "// Line comment");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, COMMENT), "//");
        assertLine(Language.FSHARP, new Line(Language.FSHARP, CODE), "let x = 3 + (4 * 5) // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "(* Print a list of numbers recursively\n"
                + "		Written in F#   *)\n"
                + "let rec printList lst =\n"
                + "		match lst with\n"
                + "		| [] -> ()\n"
                + "		| h :: t -> \n"
                + "			printf \"%d\" h\n"
                + "			printList t\n";

        Line[] expected = {
                new Line(Language.FSHARP, COMMENT),
                new Line(Language.FSHARP, COMMENT),
                new Line(Language.FSHARP, CODE),
                new Line(Language.FSHARP, CODE),
                new Line(Language.FSHARP, CODE),
                new Line(Language.FSHARP, CODE),
                new Line(Language.FSHARP, CODE),
                new Line(Language.FSHARP, CODE)
        };
        assertLines(Language.FSHARP, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "(*\n\n\n";

        Line[] expected = {
                new Line(Language.FSHARP, COMMENT),
                new Line(Language.FSHARP, BLANK),
                new Line(Language.FSHARP, BLANK)
        };
        assertLines(Language.FSHARP, expected, code);
    }
}
