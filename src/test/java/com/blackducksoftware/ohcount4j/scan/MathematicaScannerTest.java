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

public class MathematicaScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK), "\n");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK), "     \n");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK), "\t\n");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE), "While[(i<=Length[a]),x=a[[i]];\n");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, COMMENT), "(* Block comment *)\n");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE), "While[(i<=Length[a]),x=a[[i]]; (* with comment *)\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK), "     ");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, BLANK), "\t");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE), "While[(i<=Length[a]),x=a[[i]];");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, COMMENT), "(* Block comment *)");
        assertLine(Language.MATHEMATICA, new Line(Language.MATHEMATICA, CODE), "While[(i<=Length[a]),x=a[[i]]; (* with comment *)");
    }

    @Test
    public void sampleTest() {
        String code = "(* Part of a sample program\n"
                + "		written in Mathematica\n"
                + "\t\n"
                + "		(* Nested Comment *)\n"
                + "				*)\n"
                + "While[(i<=Length[a]),x=a[[i]];\n"
                + "		While[((i<=Length[a])&&(a[[i]]==x)),cnt+=1;i+=1];\n"
                + "			ls=Append[ls,cnt];\n"
                + "			cnt=0]\n"
                + "a\n"
                + "ls;\n";

        Line[] expected = {
                new Line(Language.MATHEMATICA, COMMENT),
                new Line(Language.MATHEMATICA, COMMENT),
                new Line(Language.MATHEMATICA, BLANK),
                new Line(Language.MATHEMATICA, COMMENT),
                new Line(Language.MATHEMATICA, COMMENT),
                new Line(Language.MATHEMATICA, CODE),
                new Line(Language.MATHEMATICA, CODE),
                new Line(Language.MATHEMATICA, CODE),
                new Line(Language.MATHEMATICA, CODE),
                new Line(Language.MATHEMATICA, CODE),
                new Line(Language.MATHEMATICA, CODE)
        };
        assertLines(Language.MATHEMATICA, expected, code);
    }

    @Test
    public void unterminatedNestedCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "(*\n(* *)\n\n\n";

        Line[] expected = {
                new Line(Language.MATHEMATICA, COMMENT),
                new Line(Language.MATHEMATICA, COMMENT),
                new Line(Language.MATHEMATICA, BLANK),
                new Line(Language.MATHEMATICA, BLANK)
        };
        assertLines(Language.MATHEMATICA, expected, code);
    }
}
