/* D.java
 * Created on Sat Aug  4 17:52:39 2018
 *
 * (c) 2004 MyCorp, etc. etc.
 */
%package%

    import org.apache.log4j.Logger;


/*** 
 * [Documentation Here!]
 *
 * @author
 * Michael Chyson
 * @version $Id: ch11.xml,v 1.3 2004/12/14 16:55:39 kend Exp kend $
 *
 **/
public class %class% {
    /**
     * Provides access to the CVS version of this class.
     **/
    public static final String VERSION =
        "$Id: ch11.xml,v 1.3 2004/12/14 16:55:39 kend Exp kend $";
    /**
     * Provides hierarchical control and configuration of debugging via
     * class package structure.
     **/
    private static Logger log =
        Logger.getLogger(%class%.class);
}
