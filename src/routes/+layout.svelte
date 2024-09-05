<script lang="ts">
  import "@fontsource/noto-sans";
  import "../app.css";

  const tabs = [
    { title: "Home", href: "/" },
    { title: "Contact", href: "/contact" },
  ];
</script>

<header>
  <div class="masthead">
    <h1>Jason Chen</h1>
  </div>
  <nav>
    {#each tabs as tab}
      <span>
        <a href={tab.href}>{tab.title}</a>
      </span>
    {/each}
  </nav>
</header>
<main>
  <slot />
</main>
<footer></footer>

<style>
  :root {
    --masthead-hbuffer: 0.5em;
  }

  header {
    margin-left: calc(-1 * var(--masthead-hbuffer));
    display: flex;
    justify-content: space-between;
    align-items: baseline;
  }

  .masthead {
    padding-left: var(--masthead-hbuffer);
    background-image: url("./assets/masthead.jpg");
    color: transparent;
    background-size: cover;
    background-clip: text;
    -webkit-background-clip: text;
  }

  .masthead h1 {
    display: inline;
  }
</style>
