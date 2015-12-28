package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class PerlScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.PERL, new Line(Language.PERL, BLANK), "\n");
        assertLine(Language.PERL, new Line(Language.PERL, BLANK), "     \n");
        assertLine(Language.PERL, new Line(Language.PERL, BLANK), "\t\n");
        assertLine(Language.PERL, new Line(Language.PERL, CODE), "print $string; # \"world\"\n");
        assertLine(Language.PERL, new Line(Language.PERL, COMMENT), "# Line comment\n");
        assertLine(Language.PERL, new Line(Language.PERL, COMMENT), "#\n");
        assertLine(Language.PERL, new Line(Language.PERL, CODE), "print $string; # \"world\"\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.PERL, new Line(Language.PERL, BLANK), "     ");
        assertLine(Language.PERL, new Line(Language.PERL, BLANK), "\t");
        assertLine(Language.PERL, new Line(Language.PERL, CODE), "print $string; # \"world\"");
        assertLine(Language.PERL, new Line(Language.PERL, COMMENT), "# Line comment");
        assertLine(Language.PERL, new Line(Language.PERL, COMMENT), "#");
        assertLine(Language.PERL, new Line(Language.PERL, CODE), "print $string; # \"world\" # with comment");
    }

    @Test
    public void sampleTest() {
        String code = "# Sample Code\n"
                + "# Written in Perl\n"
                + "\n"
                + "sub contextualSubroutine {\n"
                + "	# Caller wants a list. Return a list\n"
                + "	return (\"Everest\", \"K2\", \"Etna\") if wantarray;\n"
                + "\n"
                + "	# Caller wants a scalar. Return a scalar\n"
                + "	return \"Everest ::: K2 ::: Etna\";\n"
                + "}\n"
                + "=PODCOMMENT\n"
                + "POD \"Plain Old Documentation\"\n"
                + "Not proper multiline comment but is used\n"
                + "\t\n"
                + "=CUT\n"
                + "my @array = contextualSubroutine();\n"
                + "print @array; # \"EverestK2Etna\"\n"
                + "my $scalar = contextualSubroutine();\n"
                + "print $scalar; # \"Everest ::: K2 ::: Etna\"";

        Line[] expected = {
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, BLANK),
                new Line(Language.PERL, CODE),
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, CODE),
                new Line(Language.PERL, BLANK),
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, CODE),
                new Line(Language.PERL, CODE),
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, BLANK),
                new Line(Language.PERL, COMMENT),
                new Line(Language.PERL, CODE),
                new Line(Language.PERL, CODE),
                new Line(Language.PERL, CODE),
                new Line(Language.PERL, CODE)
        };
        assertLines(Language.PERL, expected, code);
    }

}
