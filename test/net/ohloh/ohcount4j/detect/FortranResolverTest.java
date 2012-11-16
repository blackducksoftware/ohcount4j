package net.ohloh.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class FortranResolverTest {

    private FortranResolver r;

    @BeforeTest()
    public void setup() {
        r = new FortranResolver();
    }

    @Test
    public void canResolvetest() {
        assertFalse(r.canResolve(Language.RUBY));
        assertTrue(r.canResolve(Language.FORTRANFIXED));
        assertTrue(r.canResolve(Language.FORTRANFREE));
    }

    @Test
    public void fiixedByDefaultTest() throws IOException {
        assertEquals(Language.FORTRANFIXED, r.resolve(new SourceFile("foo.f", "")));
    }

    @Test
    public void fixedExamplesTest() throws IOException {
        assertEquals(Language.FORTRANFIXED, r.resolve(new SourceFile("foo.f",
                "      PROGRAM fortranfixedcheck\n" +
                        "!     Simple check.  Not valid free-form because of the continuation.\n" +
                        "      WRITE(*,*)\n" +
                        "     + 'foo'\n" +
                        "      GOTO 22\n" +
                        " 22   WRITE(*,*) 'bar'\n" +
                        "      END\n")));

        assertEquals(Language.FORTRANFIXED, r.resolve(new SourceFile("foo.f",
                "C     Comment\n" +
                        "      program foo\n")));
    }

    @Test
    public void freeExamplesTest() throws IOException {
        assertEquals(Language.FORTRANFREE, r.resolve(new SourceFile("foo.f",
                "! -*- F90 -*-\n" +
                        "program fortranfreecheck\n" +
                        "!     Simple check.  Not valid fixed form thanks to code starting in first column.\n" +
                        "    write(*,*) 2 + &\n" +
                        "        & 2\n" +
                        "    goto 22\n" +
                        " 22   write(*,*) 'bar'\n" +
                        "end program fortranfreecheck\n")));

        assertEquals(Language.FORTRANFREE, r.resolve(new SourceFile("foo.f",
                "! -*- F90 -*-\n" +
                        "!     Comment\n" +
                        "      program foo")));

        assertEquals(Language.FORTRANFREE, r.resolve(new SourceFile("foo.f",
                "C = 1 ! Not a comment")));
    }
}
