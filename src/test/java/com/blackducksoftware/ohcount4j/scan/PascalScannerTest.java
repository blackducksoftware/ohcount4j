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

public class PascalScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.PASCAL, new Line(Language.PASCAL, BLANK), "\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, BLANK), "     \n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, BLANK), "\t\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, CODE), "Infile_name := 'A:\' + 'PROG006' + '.OUT';\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "{ Bracket Comment }\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "(* Starparen Comment *)\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "{ (* Nested Comment *) }\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "(* { Nested Comment } *)\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "// Line comment\n");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, CODE), "File_name = String[14]; { with comment }\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.PASCAL, new Line(Language.PASCAL, BLANK), "     ");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, BLANK), "\t");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, CODE), "Infile_name := 'A:\' + 'PROG006' + '.OUT';");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "{ Bracket Comment }");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "(* Starparen Comment *)");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "{ (* Nested Comment *) }");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "(* { Nested Comment } *)");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, COMMENT), "// Line comment");
        assertLine(Language.PASCAL, new Line(Language.PASCAL, CODE), "File_name = String[14]; { with comment }");
    }

    @Test
    public void simpleTest() {
        String code = "{\n"
                + "multi-line comment\n"
                + "\n"
                + "}\n;"
                + "File_name = String[14]; (* with multi\n"
                + "\t\n"
                + "line comment *)\n"
                + "{ comment (* nested comment *) more comment }";

        Line[] expected = {
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, BLANK),
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, CODE),
                new Line(Language.PASCAL, BLANK),
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, COMMENT)
        };
        assertLines(Language.PASCAL, expected, code);
    }

    @Test
    public void unterminatedBracketBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "{\n\n\n";

        Line[] expected = {
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, BLANK),
                new Line(Language.PASCAL, BLANK)
        };
        assertLines(Language.PASCAL, expected, code);
    }

    @Test
    public void unterminatedStarparenBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "(*\n\n\n";

        Line[] expected = {
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, BLANK),
                new Line(Language.PASCAL, BLANK)
        };
        assertLines(Language.PASCAL, expected, code);
    }

    @Test
    public void unterminatedNestedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "{\n(*\n\n\n*)\n";

        Line[] expected = {
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, COMMENT),
                new Line(Language.PASCAL, BLANK),
                new Line(Language.PASCAL, BLANK),
                new Line(Language.PASCAL, COMMENT)
        };
        assertLines(Language.PASCAL, expected, code);
    }

}
