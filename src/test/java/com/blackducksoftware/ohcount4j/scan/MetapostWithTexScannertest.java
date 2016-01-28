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

import java.io.File;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

/**
 * @author gandhip
 *
 */
public class MetapostWithTexScannertest extends BaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.METAPOST, new Line(Language.METAPOST, BLANK), "\n");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, BLANK), "     \n");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, BLANK), "\t\n");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, CODE), "u:=25;\n");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, CODE), "beginfig(1)\n");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, COMMENT), "% --- Grid ---\n");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, CODE), "draw (0, 0)--(breite, 0)--(breite, hoehe)--(0, hoehe)--cycle;\n");
        assertLine(Language.METAPOST, new Line(Language.TEX, CODE), "label.ulft(btex \\cyr C\\char24 rih, 08.09.2002 etex, (breite, 0));\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.METAPOST, new Line(Language.METAPOST, BLANK), "     ");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, BLANK), "\t");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, CODE), "u:=25;");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, CODE), "beginfig(1)");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, COMMENT), "% --- Grid ---");
        assertLine(Language.METAPOST, new Line(Language.METAPOST, CODE), "draw (0, 0)--(breite, 0)--(breite, hoehe)--(0, hoehe)--cycle;");
        assertLine(Language.METAPOST, new Line(Language.TEX, CODE), "label.ulft(btex \\cyr C\\char24 rih, 08.09.2002 etex, (breite, 0));");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("metapost.mp")));
        Line[] expected = {
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, CODE),
                new Line(Language.TEX, COMMENT),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, COMMENT),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.TEX, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.TEX, CODE),
                new Line(Language.METAPOST, CODE),
                new Line(Language.METAPOST, BLANK),
                new Line(Language.METAPOST, CODE)
        };
        assertLines(Language.METAPOST, expected, sourceFile);
    }

}
