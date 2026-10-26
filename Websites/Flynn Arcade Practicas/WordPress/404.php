<?php
defined('ABSPATH') || exit;
get_header();
?>
<main id="main-content" class="section-inner">
    <div class="page-heading"><h1>Page <span>not found.</span></h1></div>
    <div class="wordpress-content"><p>This page could not be found.</p><a class="arcade-button" href="<?php echo esc_url(flynns_page_url('home')); ?>">Back to Home</a></div>
</main>
<?php get_footer(); ?>
