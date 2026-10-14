<?php
/** Site footer. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }
?>
    <footer class="site-footer">
        <div class="footer-inner">
            <?php flynns_arcade_neon_brand(true); ?>
            <div class="footer-location">
                <?php flynns_arcade_neon_icon('gamepad2'); ?>
                <p>Developing &amp; publishing<small>Original and third-party games.</small></p>
            </div>
            <?php flynns_arcade_neon_nav('footer-nav'); ?>
            <a class="footer-contact" href="#press" data-dialog="press"><?php flynns_arcade_neon_icon('mail'); ?><?php echo esc_html(flynns_arcade_neon_editorial()['contact_email']); ?></a>
        </div>
    </footer>
</div>
<?php wp_footer(); ?>
</body>
</html>
