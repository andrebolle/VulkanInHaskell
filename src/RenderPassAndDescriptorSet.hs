{-# LANGUAGE DuplicateRecordFields #-}

--A render pass represents a collection of attachments, subpasses, and dependencies between the subpasses, and describes
--how the attachments are used over the course of the subpasses.

module RenderPassAndDescriptorSet (createRenderPass) where
--module RenderPass where

import Data.Bits ((.|.))

import Graphics.Vulkan

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)


import GHC.Word

-- This renderpass has 1 subpass
createRenderPass :: VkDevice -> IO VkRenderPass
createRenderPass device = 
      allocaArray (length attachments) $ \attachmentsArrayPtr -> do
        pokeArray attachmentsArrayPtr attachments
        with colourAttachmentRef $ \colourAttachmentRefPtr -> 
          with depthAttachmentRef $ \depthAttachmentRefPtr -> do
            let vkSubpassDescription = subpassDescription colourAttachmentRefPtr depthAttachmentRefPtr
            with subpassDependency $ \subpassDependencyPtr ->
              with vkSubpassDescription $ \subpassDescriptionPtr -> do
                  let vkRenderPassInfo = renderPassInfo 2 attachmentsArrayPtr 1 subpassDescriptionPtr 1 subpassDependencyPtr
                  with vkRenderPassInfo $ \vkRenderPassInfoPtr -> do
                    alloca $ \renderPassPtr -> do
                      vkCreateRenderPass device vkRenderPassInfoPtr nullPtr renderPassPtr
                      peek renderPassPtr

-- ************************************* Attachment Descriptions **************************************************
--An attachment description describes the properties of an attachment including its format, sample count, and how its
--contents are treated at the beginning and end of each render pass instance.

-- This colour attachment uses VK_FORMAT_B8G8R8A8_UNORM
colourAttachment :: VkAttachmentDescription
colourAttachment =   
  VkAttachmentDescription
   { vkFlags          = VkAttachmentDescriptionFlagBits 0         -- VkAttachmentDescriptionFlags 
   , vkFormat         = VK_FORMAT_B8G8R8A8_UNORM                  -- VkFormat 
   , vkSamples        = VK_SAMPLE_COUNT_1_BIT                     -- VkSampleCountFlagBits 
   , vkLoadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR               -- VkAttachmentLoadOp 
   , vkStoreOp        = VK_ATTACHMENT_STORE_OP_STORE              -- VkAttachmentStoreOp 
   , vkStencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE           -- VkAttachmentLoadOp 
   , vkStencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE          -- VkAttachmentStoreOp 
   , vkInitialLayout  = VK_IMAGE_LAYOUT_UNDEFINED                 -- VkImageLayout 
   , vkFinalLayout    = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL  -- VkImageLayout 
   }

-- This depth attachment uses VK_FORMAT_D32_SFLOAT
depthAttachment :: VkAttachmentDescription
depthAttachment =   
  VkAttachmentDescription
   { vkFlags          = VkAttachmentDescriptionFlagBits 0                 -- VkAttachmentDescriptionFlags 
   , vkFormat         = VK_FORMAT_D32_SFLOAT                              -- VkFormat 
   , vkSamples        = VK_SAMPLE_COUNT_1_BIT                             -- VkSampleCountFlagBits 
   , vkLoadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR                       -- VkAttachmentLoadOp 
   , vkStoreOp        = VK_ATTACHMENT_STORE_OP_STORE                      -- VkAttachmentStoreOp 
   , vkStencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE                   -- VkAttachmentLoadOp 
   , vkStencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE                  -- VkAttachmentStoreOp 
   , vkInitialLayout  = VK_IMAGE_LAYOUT_UNDEFINED                         -- VkImageLayout 
   , vkFinalLayout    = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL  -- VkImageLayout 
   }

--attachments :: [VkAttachmentDescription]
attachments :: [VkAttachmentDescription]
attachments = [colourAttachment, depthAttachment]

colourAttachmentRef :: VkAttachmentReference
colourAttachmentRef =
  VkAttachmentReference 
  { vkAttachment = 0
  , vkLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  }

depthAttachmentRef :: VkAttachmentReference
depthAttachmentRef =
  VkAttachmentReference 
  { vkAttachment = 0
  , vkLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  }

-- This subpass uses 1 colour and (therefore) 1 depth attachment
subpassDescription :: Ptr VkAttachmentReference -> Ptr VkAttachmentReference -> VkSubpassDescription
subpassDescription colorAttachmentRefPtr depthAttachmentRefPtr = VkSubpassDescription
  { vkFlags                   = VkSubpassDescriptionFlags 0
  , vkPipelineBindPoint       = VK_PIPELINE_BIND_POINT_GRAPHICS
  , vkInputAttachmentCount    = 0
  , vkPInputAttachments       = nullPtr
  , vkColorAttachmentCount    = 1
  , vkPColorAttachments       = colorAttachmentRefPtr
  , vkPResolveAttachments     = nullPtr
  , vkPDepthStencilAttachment = depthAttachmentRefPtr
  , vkPreserveAttachmentCount = 0
  , vkPPreserveAttachments    = nullPtr
  }

subpassDependency :: VkSubpassDependency
subpassDependency = VkSubpassDependency
  { vkSrcSubpass      = VK_SUBPASS_EXTERNAL
  , vkDstSubpass      = 0
  , vkSrcStageMask    = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
  , vkDstStageMask    = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
  , vkSrcAccessMask   = VkAccessFlagBits 0
  , vkDstAccessMask   = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  , vkDependencyFlags = VkDependencyFlagBits 0
  }

renderPassInfo :: Word32 -> Ptr VkAttachmentDescription -> Word32 -> Ptr VkSubpassDescription -> Word32 -> Ptr VkSubpassDependency -> VkRenderPassCreateInfo
renderPassInfo attCount attachmentsPtr subCount subpassesPtr depCount dependenciesPtr = VkRenderPassCreateInfo
  { vkSType             = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO         -- :: VkStructureType 
  , vkPNext             = nullPtr                                           -- :: Ptr Void 
  , vkFlags             = VkRenderPassCreateFlags 0                         -- :: VkRenderPassCreateFlags 
  , vkAttachmentCount   = attCount                                          -- :: Word32 
  , vkPAttachments      = attachmentsPtr                                    -- :: Ptr VkAttachmentDescription 
  , vkSubpassCount      = subCount                                          -- :: Word32 
  , vkPSubpasses        = subpassesPtr                                      -- :: Ptr VkSubpassDescription 
  , vkDependencyCount   = depCount                                          -- :: Word32 
  , vkPDependencies     = dependenciesPtr                                   -- :: Ptr VkSubpassDependency 
  }


-- ************************************* Descriptor Set **************************************************

createDescriptorSetLayout device = 
      allocaArray (length bindings) $ \bindingsArrayPtr -> do
        pokeArray bindingsArrayPtr bindings
        with (descriptorSetLayoutCreateInfo bindingsArrayPtr) $ \descriptorSetLayoutCreatePtr -> do
          alloca $ \descriptorSetLayoutPtr -> do
            vkCreateDescriptorSetLayout device descriptorSetLayoutCreatePtr nullPtr descriptorSetLayoutPtr
            peek descriptorSetLayoutPtr

descriptorSetLayoutCreateInfo bindingsPtr = VkDescriptorSetLayoutCreateInfo
  { vkSType         = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO   -- :: VkStructureType 
  , vkPNext         = nullPtr                                               -- :: Ptr Void 
  , vkFlags         = VkDescriptorSetLayoutCreateFlags 0                    -- :: VkDescriptorSetLayoutCreateFlags 
  , vkBindingCount  = fromIntegral (length bindings)                        -- :: Word32 
  , vkPBindings     = bindingsPtr                                           -- :: Ptr VkDescriptorSetLayoutBinding 
  }

bindings = [uboLayoutBinding, samplerLayoutBinding]

uboLayoutBinding =  VkDescriptorSetLayoutBinding
  { vkBinding               = 0                                             -- :: Word32 
  , vkDescriptorType        = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER             -- :: VkDescriptorType 
  , vkDescriptorCount       = 1                                             -- :: Word32 
  , vkStageFlags            = VK_SHADER_STAGE_VERTEX_BIT                    -- :: VkShaderStageFlags 
  , vkPImmutableSamplers    = nullPtr                                       -- :: Ptr VkSampler 
  }

samplerLayoutBinding =  VkDescriptorSetLayoutBinding
  { vkBinding               = 1                                             -- :: Word32 
  , vkDescriptorType        = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER     -- :: VkDescriptorType 
  , vkDescriptorCount       = 1                                             -- :: Word32 
  , vkStageFlags            = VK_SHADER_STAGE_FRAGMENT_BIT                  -- :: VkShaderStageFlags 
  , vkPImmutableSamplers    = nullPtr                                       -- :: Ptr VkSampler 
  }
