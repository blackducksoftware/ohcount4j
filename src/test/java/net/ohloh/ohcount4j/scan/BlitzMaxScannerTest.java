/**
 * Copyright (C) 2015 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;

import java.io.File;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.Test;

/**
 * @author gandhip
 *
 */
public class BlitzMaxScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "     \n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "\t\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "SuperStrict\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Import BRL.StandardIO\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, COMMENT), "' comment\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "print \"hello rem fish ' \"\n");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Type TABC \n");
    }

    @Test
    public void helloWorld() {
        String code = "Rem\n" // Multi-line comment
                + "bbdoc: docs\n"
                + "End Rem\n"
                + "\n"
                + "Import BRL.StandardIO\n"
                + "\n"
                + "\tMethod hello() \n"
                + "' single line comment \n" // single line comment
                + "\t\tEnd Method\n";
        Line[] expected = {
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, CODE),
        };
        assertLines(Language.BLITZMAX, expected, code);
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "     ");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "     ");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, BLANK), "\t");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "SuperStrict");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Import BRL.StandardIO");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, COMMENT), "' comment");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "print \"hello rem fish ' \"");
        assertLine(Language.BLITZMAX, new Line(Language.BLITZMAX, CODE), "Type TABC ");
    }

    @Test
    public void sampleTestWithSourceFile() throws Exception {
        SourceFile sourceFile = new SourceFile(new File(getSourceCodePath("blitzmax.bmx")));
        Line[] expected = {
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, COMMENT),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE),
                new Line(Language.BLITZMAX, BLANK),
                new Line(Language.BLITZMAX, CODE)
        };
        assertLines(Language.BLITZMAX, expected, sourceFile);
    }

}
