<?php
/** Social, newsletter and contact sections in the original order. @package FlynnsArcadeNeon */
if (!defined('ABSPATH')) { exit; }
$footer = flynns_arcade_neon_data('site')['footer'];
$contact = flynns_arcade_neon_editorial()['contact_email'];
?>
<footer class="site-footer">
    <section class="social-band"><div class="section-inner social-band-inner"><div><h2><?php echo esc_html($footer['social_title']); ?></h2><p><?php echo esc_html($footer['social_copy']); ?></p></div><?php flynns_arcade_neon_socials(); ?></div></section>
    <div class="contact-band"><div class="section-inner contact-columns">
        <section class="newsletter" aria-labelledby="newsletter-title"><h2 id="newsletter-title"><?php echo esc_html($footer['newsletter_title']); ?></h2><p><?php echo esc_html($footer['newsletter_copy']); ?></p><p><?php echo esc_html($footer['newsletter_extra']); ?></p><form class="newsletter-form" data-newsletter><label class="screen-reader-text" for="newsletter-email">Your email address</label><input id="newsletter-email" name="email" type="email" autocomplete="email" placeholder="Your email address" required><button type="submit">Subscribe <?php flynns_arcade_neon_icon('arrow-right'); ?></button></form><p class="form-note" data-newsletter-note role="status" hidden></p></section>
        <section class="contact" aria-labelledby="contact-title"><h2 id="contact-title"><?php echo esc_html($footer['contact_title']); ?></h2><p><?php echo esc_html($footer['contact_copy']); ?></p><p><?php echo esc_html($footer['contact_extra']); ?></p><a class="contact-email" href="<?php echo esc_url('mailto:' . $contact); ?>"><?php flynns_arcade_neon_icon('mail'); ?><span><?php echo esc_html($contact); ?></span></a></section>
    </div></div>
    <div class="section-inner footer-bottom"><span>Flynn's Arcade</span><span>Independent games. Arcade soul.</span><a href="<?php echo esc_url(flynns_arcade_neon_url('about')); ?>">About the studio <?php flynns_arcade_neon_icon('arrow-right'); ?></a></div>
</footer>
<?php wp_footer(); ?>
</body></html>
