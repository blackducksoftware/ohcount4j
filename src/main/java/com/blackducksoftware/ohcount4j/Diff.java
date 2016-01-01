/*
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
package com.blackducksoftware.ohcount4j;

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * @author mpujari
 *
 */
public class Diff {

    private int codeLinesAdded;

    private int codeLinesRemoved;

    private int commentLinesAdded;

    private int commentLinesRemoved;

    private int blankLinesAdded;

    private int blankLinesRemoved;

    public Diff(int codeLinesAdded, int codeLinesRemoved, int commentLinesAdded, int commentLinesRemoved,
            int blankLinesAdded, int blankLinesRemoved) {
        this.codeLinesAdded = codeLinesAdded;
        this.codeLinesRemoved = codeLinesRemoved;
        this.commentLinesAdded = commentLinesAdded;
        this.commentLinesRemoved = commentLinesRemoved;
        this.blankLinesAdded = blankLinesAdded;
        this.blankLinesRemoved = blankLinesRemoved;
    }

    public int getCodeLinesAdded() {
        return codeLinesAdded;
    }

    public int getCodeLinesRemoved() {
        return codeLinesRemoved;
    }

    public int getCommentLinesAdded() {
        return commentLinesAdded;
    }

    public int getCommentLinesRemoved() {
        return commentLinesRemoved;
    }

    public int getBlankLinesAdded() {
        return blankLinesAdded;
    }

    public int getBlankLinesRemoved() {
        return blankLinesRemoved;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Diff)) {
            return false;
        }
        Diff t = (Diff) obj;
        return t.codeLinesAdded == codeLinesAdded && t.codeLinesRemoved == codeLinesRemoved
                && t.commentLinesAdded == commentLinesAdded && t.commentLinesRemoved == commentLinesRemoved
                && t.blankLinesAdded == blankLinesAdded && t.blankLinesRemoved == blankLinesRemoved;
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(codeLinesAdded).append(codeLinesRemoved)
                .append(commentLinesAdded).append(commentLinesRemoved)
                .append(blankLinesAdded).append(blankLinesRemoved).toHashCode();
    }

    @Override
    public String toString() {
        return String.format("Code added:%d, Code removed:%d, Comment added:%d, Comment removed:%d, Blank added:%d, Blank removed:%d",
                codeLinesAdded, codeLinesRemoved, commentLinesAdded,
                commentLinesRemoved, blankLinesAdded, blankLinesRemoved);
    }

}
