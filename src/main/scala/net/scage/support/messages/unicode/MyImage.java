package net.scage.support.messages.unicode;

import org.newdawn.slick.SlickException;

public class MyImage extends org.newdawn.slick.Image
{
    public MyImage()
    {
        super();
    }

    public MyImage(int width, int height) throws SlickException
    {
        super(width, height);
    }

    @Override protected void initImpl()
    {

    }

    public void drawEmbedded(float x, float y, float width, float height, int size)
    {
        init();
        if (corners == null) {
            GL.glTexCoord2f(textureOffsetX, textureOffsetY);
			GL.glVertex3f(x, -y + size, 0);

			GL.glTexCoord2f(textureOffsetX, textureOffsetY + textureHeight);
			GL.glVertex3f(x, -y - height + size, 0);

			GL.glTexCoord2f(textureOffsetX + textureWidth, textureOffsetY + textureHeight);
			GL.glVertex3f(x + width, -y - height + size, 0);

			GL.glTexCoord2f(textureOffsetX + textureWidth, textureOffsetY);
			GL.glVertex3f(x + width, -y + size, 0);
		} else {
			corners[TOP_LEFT].bind();
		    GL.glTexCoord2f(textureOffsetX, textureOffsetY);
			GL.glVertex3f(x, y, 0);

            corners[BOTTOM_LEFT].bind();
			GL.glTexCoord2f(textureOffsetX, textureOffsetY + textureHeight);
			GL.glVertex3f(x, y + height, 0);

            corners[BOTTOM_RIGHT].bind();
			GL.glTexCoord2f(textureOffsetX + textureWidth, textureOffsetY + textureHeight);
			GL.glVertex3f(x + width, y + height, 0);

            corners[TOP_RIGHT].bind();
			GL.glTexCoord2f(textureOffsetX + textureWidth, textureOffsetY);
			GL.glVertex3f(x + width, y, 0);




		}
    }

    @Override public MyImage getSubImage(int x,int y,int width,int height) {
		init();

		float newTextureOffsetX = ((x / (float) this.width) * textureWidth) + textureOffsetX;
		float newTextureOffsetY = ((y / (float) this.height) * textureHeight) + textureOffsetY;
		float newTextureWidth = ((width / (float) this.width) * textureWidth);
		float newTextureHeight = ((height / (float) this.height) * textureHeight);

		MyImage sub = new MyImage();
		sub.inited = true;
		sub.texture = this.texture;
		sub.textureOffsetX = newTextureOffsetX;
		sub.textureOffsetY = newTextureOffsetY;
		sub.textureWidth = newTextureWidth;
		sub.textureHeight = newTextureHeight;

		sub.width = width;
		sub.height = height;
		sub.ref = ref;
		sub.centerX = width / 2;
		sub.centerY = height / 2;

		return sub;
	}
}
